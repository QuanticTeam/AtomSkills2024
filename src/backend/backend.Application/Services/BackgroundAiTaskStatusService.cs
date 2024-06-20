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
        Console.WriteLine("SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS");
        Console.WriteLine("SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS");
        Console.WriteLine("SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS");
        Console.WriteLine("SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS");
        Console.WriteLine("SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS");
        Console.WriteLine("SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS");
        Console.WriteLine("SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS");
        Console.WriteLine("SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS");
        Console.WriteLine("SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS");


        using var scope = _serviceProvider.CreateScope();

        var taskStatusesRepository = scope.ServiceProvider.GetService<ITaskStatusesRepository>()!;
        var minIoFileService = scope.ServiceProvider.GetService<IMinIoFileService>()!;
        var defectsRepository = scope.ServiceProvider.GetService<IDefectsRepository>();
        var taskStatuses = await taskStatusesRepository.Get();

        var status = taskStatuses
            .FirstOrDefault(x => x.Status.Equals(TaskStatusType.SendToCheck.ToString()));
        
        if (status == null)
            return;

        var defectCounts = 0;

        Console.WriteLine("PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP");
        Console.WriteLine("PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP");
        Console.WriteLine("PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP");
        Console.WriteLine("PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP");
        Console.WriteLine($"{status.Id} -------------- {status.Fotos.Count()}");


        foreach (var foto in status.Fotos)
        {
            try
            {
                Console.WriteLine("DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD");
                Console.WriteLine("DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD");

                var (file, fileName, fileKey) = await minIoFileService.Download(foto.Key);

                // var jsonDefects = await _aiClient.CheckImage(
                //     file, fileName, cancellationToken
                // );

                Console.WriteLine("DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD");
                Console.WriteLine("DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD");


                var jsonDefects = new List<JsonDefect> {
                    new JsonDefect
                    {
                        Area = new Area { X1 = 50, X2 = 80, Y1 = 500, Y2 = 800 },
                        Features = new List<string> { "DT Defect 1", "DT Defect 2" }.ToArray(),
                    }
                };

                Console.WriteLine("DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD");
                Console.WriteLine("DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD");


                var adjusted = AdjustImage(file, jsonDefects);
                new FileExtensionContentTypeProvider().TryGetContentType(
                    fileName, out var mimeType);
                var newFileKey = fileName.GetMinIoFileName();

                var fileModel = new MinIoFileModel(
                    mimeType ?? string.Empty,
                    newFileKey,
                    adjusted,
                    new Dictionary<string, string> {}
                );
                await minIoFileService.Upload(fileModel);

                Console.WriteLine("DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD");
                Console.WriteLine("DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD");


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

                Console.WriteLine("DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD");
                Console.WriteLine("DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD");

                foreach (var defect in defects)
                {
                    await defectsRepository!.Create(defect);
                }

                Console.WriteLine("DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD");
                Console.WriteLine("DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD");

            }
            catch (Exception e)
            {
                Console.WriteLine("EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE");
                Console.WriteLine("EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE");
                Console.WriteLine("EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE");
                Console.WriteLine("EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE");
                Console.WriteLine("EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE");
                Console.WriteLine("EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE");
                Console.WriteLine("EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE");
                Console.WriteLine("EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE");
                Console.WriteLine("EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE");
                Console.WriteLine("EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE");
                Console.WriteLine("EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE");
                Console.WriteLine("EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE");
                Console.WriteLine(e.Message);
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
                
                Console.WriteLine(e.Message);
                return;
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
        Console.WriteLine("ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ");

        var returnMemory = new MemoryStream();

        foreach (var defect in defects)
        {
            var ((x1, y1, x2, y2), _) = defect;

            using var original = SKBitmap.Decode(msImage);
            using var image = SKImage.FromBitmap(original);
            Console.WriteLine("ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ");

            using var surface = SKSurface.Create(new SKImageInfo(image.Width, image.Height));
            {
                Console.WriteLine("ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ");
                var canvas = surface.Canvas;

                Console.WriteLine("ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ");
                // Копирование оригинального изображения на холст
                canvas.DrawImage(image, 0, 0);

                Console.WriteLine("ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ");
                // Определение параметров кисти
                var paint = new SKPaint
                {
                    Color = SKColors.Red,
                    IsStroke = true,
                    StrokeWidth = 2
                };

                // Рисование прямоугольника
                canvas.DrawRect(new SKRect(x1, y1, x2, y2), paint);
                
                // Сохранение изображения
                surface.Snapshot().Encode(SKEncodedImageFormat.Png, 100).SaveTo(returnMemory);
            }
        }

        return returnMemory;
    }
}