using System.Net.Http.Headers;
using backend.Core.Abstractions;
using backend.Core.JsonModels;
using backend.Core.Models;
using backend.Core.Options;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Options;
using Microsoft.IdentityModel.Protocols.Configuration;
using Microsoft.IdentityModel.Tokens;
using Newtonsoft.Json;
using Task = System.Threading.Tasks.Task;
using TaskStatus = backend.Core.Models.TaskStatus;

namespace backend.Application.Services;

public class BackgroundAiTaskStatusService : BackgroundService
{
    private readonly IServiceProvider _serviceProvider;
    private readonly AiOptions _aiOptions;

    public BackgroundAiTaskStatusService(IServiceProvider serviceProvider, IOptions<AiOptions> options)
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
            await Task.Delay(TimeSpan.FromSeconds(5), cancellationToken).ConfigureAwait(false);
        } 
    }

    private async Task SendToAiAsync(CancellationToken cancellationToken)
    {
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

        foreach (var foto in status.Fotos)
        {
            try
            {
                var file = await minIoFileService.Download(foto.Key);
                
                using var client = new HttpClient { BaseAddress = new Uri($"{_aiOptions.AS_2024_ENV_HOST}:{_aiOptions.AS_2024_ENV_PORT}") };
                
                using var form = new MultipartFormDataContent();
                var fileContent = new StreamContent(file.Item1);
                fileContent.Headers.ContentType = MediaTypeHeaderValue.Parse("multipart/form-data");
                form.Add(fileContent, "file", file.Item2);
        
                //response
                var response = await client.PostAsync("/check", form, cancellationToken);
                response.EnsureSuccessStatusCode();
                var result = await response.Content.ReadAsStringAsync(cancellationToken);
            
                // parsing
                var jsonDefects = JsonConvert.DeserializeObject<List<JsonDefect>>(result);
                var defects = jsonDefects.Select(x => new Defect
                {
                    FileKey = file.Item3,
                    Codes = x.Features,
                    Comment = string.Empty,
                    X1 = x.Area.X1,
                    Y1 = x.Area.Y1,
                    X2 = x.Area.X2,
                    Y2 = x.Area.Y2,
                    TaskStatusRecordId = status.Id,
                }).ToList();

                defectCounts += defects.Count;
                
                // db
                foreach (var defect in defects)
                {
                    await defectsRepository!.Create(defect);
                }
                
            }
            catch (Exception e)
            {
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
}