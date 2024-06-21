using backend.Core.Abstractions;
using backend.Core.JsonModels;
using backend.Core.Models;
using backend.Core.Options;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Options;
using Microsoft.IdentityModel.Protocols.Configuration;
using Microsoft.IdentityModel.Tokens;
using Task = System.Threading.Tasks.Task;
using TaskStatus = backend.Core.Models.TaskStatus;
using SkiaSharp;
using Microsoft.AspNetCore.StaticFiles;
using backend.Application.Extensions;

namespace backend.Application.Services;

public class BackgroundAiTaskStatusService : BackgroundService
{
    private readonly IServiceProvider _serviceProvider;
    private readonly AiOptions _aiOptions;

    public BackgroundAiTaskStatusService(
        IServiceProvider serviceProvider,
        IOptions<AiOptions> options)
    {
        if (options.Value.AS_2024_ENV_AI_OFF)
        {
            base.StopAsync(default);
        }

        _serviceProvider = serviceProvider;
        _aiOptions = options.Value;

        if (_aiOptions.AS_2024_ENV_HOST.IsNullOrEmpty())
            throw new InvalidConfigurationException($"Опция '{nameof(_aiOptions.AS_2024_ENV_HOST)}' должна быть добавлена в конфиг");

        if (_aiOptions.AS_2024_ENV_DICT_DIR.IsNullOrEmpty())
            throw new InvalidConfigurationException($"Опция '{nameof(_aiOptions.AS_2024_ENV_DICT_DIR)}' должна быть добавлена в конфиг");

        Thread.Sleep(TimeSpan.FromSeconds(15));
    }

    protected override async Task ExecuteAsync(CancellationToken cancellationToken) 
    { 
        while (!cancellationToken.IsCancellationRequested) 
        { 
            await SendToAiAsync(cancellationToken);
            await Task.Delay(TimeSpan.FromSeconds(15), cancellationToken).ConfigureAwait(false);
        } 
    }

    private async Task SendToAiAsync(CancellationToken cancellationToken)
    {
        using var scope = _serviceProvider.CreateScope();

        var taskStatusesRepository = scope.ServiceProvider.GetService<ITaskStatusesRepository>()!;
        var minIoFileService = scope.ServiceProvider.GetService<IMinIoFileService>()!;
        var defectsRepository = scope.ServiceProvider.GetService<IDefectsRepository>();
        var fotoRepository = scope.ServiceProvider.GetService<IFotosRepository>();
        var taskStatuses = await taskStatusesRepository.Get();

        var status = taskStatuses
            .FirstOrDefault(x => x.Status.Equals(TaskStatusType.SendToCheck.ToString()));
        
        if (status == null)
            return;

        var defectCounts = 0;

        foreach (var foto in status.Fotos)
        {
            try
            {
                var (file, fileName, fileKey) = await minIoFileService.Download(foto.Key);

                // var jsonDefects = await _aiClient.CheckImage(
                //     file, fileName, cancellationToken
                // );

                var jsonDefects = new List<JsonDefect> {
                    new JsonDefect
                    {
                        Area = new Area { X1 = 50, X2 = 80, Y1 = 500, Y2 = 800 },
                        Features = new List<string> { "DT Defect 1", "DT Defect 2" }.ToArray(),
                    }
                };

                var adjusted = AdjustImage(file, jsonDefects);
                new FileExtensionContentTypeProvider().TryGetContentType(
                    fileName, out var mimeType);
                var newFileKey = fileName.GetMinIoFileName();

                var fileModel = new MinIoFileModel(
                    newFileKey,
                    mimeType ?? string.Empty,
                    adjusted,
                    new Dictionary<string, string> {
                        { "x-amz-meta-original-file-name", fileName }
                    }
                );
                await minIoFileService.Upload(fileModel);

                Console.WriteLine($"newFileKey: {newFileKey}");

                var defects = jsonDefects.Select(x => new Defect
                {
                    FileKey = newFileKey,
                    Codes = x.Features,
                    Comment = string.Empty,
                    X1 = x.Area.X1,
                    Y1 = x.Area.Y1,
                    X2 = x.Area.X2,
                    Y2 = x.Area.Y2,
                    TaskStatusRecordId = status.Id,
                }).ToList();

                defectCounts += defects.Count;

                foreach (var defect in defects)
                {
                    defect.FileKey = newFileKey;
                    await defectsRepository!.Create(defect);
                }

                foto.Key = newFileKey;
                var response = await fotoRepository!.Create(new List<Foto> {foto}, status.Id);
            }
            catch (Exception e)
            {
                Console.WriteLine(e);

                var errorAiStatus = new TaskStatus
                {
                    Id = status.Id,
                    Status = TaskStatusType.AiVerified.ToString(),
                    AutomationSystemStatus = AutomationSystemStatus.Error.ToString(),
                    StartedAt = status.StartedAt,
                    FinishedAt = status.FinishedAt,
                    Mark = status.Mark,
                }; 
                
                await taskStatusesRepository.Update(errorAiStatus);
            }
        }

        var updateTaskStatus = new TaskStatus
        {
            Id = status.Id,
            Status = TaskStatusType.AiVerified.ToString(),
            AutomationSystemStatus = AutomationSystemStatus.Complete.ToString(),
            StartedAt = status.StartedAt,
            FinishedAt = status.FinishedAt,
            Mark = defectCounts == 0 ? 5 :
                   defectCounts < 3 ? 4 :
                   defectCounts < 5 ? 3 :
                   2,
        }; 
        
        await taskStatusesRepository.Update(updateTaskStatus);
    }

    private MemoryStream AdjustImage(MemoryStream msImage, List<JsonDefect> defects)
    {
        var returnMemory = new MemoryStream();

        foreach (var defect in defects)
        {
            var ((x1, y1, x2, y2), _) = defect;

            using var original = SKBitmap.Decode(msImage);
            using var image = SKImage.FromBitmap(original);

            using var surface = SKSurface.Create(new SKImageInfo(image.Width, image.Height));
            {
                var canvas = surface.Canvas;

                // Копирование оригинального изображения на холст
                canvas.DrawImage(image, 0, 0);

                // Определение параметров кисти
                var paint = new SKPaint
                {
                    Color = SKColors.Red,
                    IsStroke = true,
                    StrokeWidth = 3,
                };

                // Рисование прямоугольника
                canvas.DrawRect(new SKRect(x1, y1, x2, y2), paint);
                
                // Сохранение изображения
                surface
                    .Snapshot()
                    .Encode(SKEncodedImageFormat.Png, 100)
                    .SaveTo(returnMemory);
            }
        }

        returnMemory.Seek(0, SeekOrigin.Begin);

        return returnMemory;
    }
}