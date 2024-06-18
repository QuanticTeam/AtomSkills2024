using System.Net.Http.Headers;
using backend.Core.Abstractions;
using backend.Core.Models;
using backend.Core.Options;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Options;
using Task = System.Threading.Tasks.Task;
using TaskStatus = backend.Core.Models.TaskStatus;

namespace backend.Application.Services;

public class BackgroundTaskStatusService : BackgroundService
{
    private readonly IServiceProvider _serviceProvider;
    private readonly AiOptions _aiOptions;

    public BackgroundTaskStatusService(IServiceProvider serviceProvider, IOptions<AiOptions> options)
    {
        _serviceProvider = serviceProvider;
        _aiOptions = options.Value;
        Thread.Sleep(TimeSpan.FromSeconds(5));
    }

    protected override async Task ExecuteAsync(CancellationToken cancellationToken) 
    { 
        while (!cancellationToken.IsCancellationRequested) 
        { 
            await SendExpiredAsync(cancellationToken);
            //await SendToAiAsync(cancellationToken);
            await SendToRecommendedAsync(cancellationToken);
            await Task.Delay(TimeSpan.FromSeconds(5), cancellationToken).ConfigureAwait(false);
        } 
    }

    private async Task SendExpiredAsync(CancellationToken cancellationToken)
    {
        var time = DateTime.UtcNow;
        using var scope = _serviceProvider.CreateScope();
        var taskStatusesRepository = scope.ServiceProvider.GetService<ITaskStatusesRepository>()!;
        var tasksRepository = scope.ServiceProvider.GetService<ITasksRepository>()!;
        var tasks = await tasksRepository.Get();
        var taskStatuses = await taskStatusesRepository.Get();

        var expiredTaskStatuses = taskStatuses
            .Where(x => x.Status.Equals(TaskStatusType.InWork.ToString()) && (time - x.StartedAt).Minutes > tasks
                .FirstOrDefault(t => t.Code.Equals(x.TaskCode))!.Time).ToList();

        foreach (var taskStatus in expiredTaskStatuses)
        {
            var updateTaskStatus = new TaskStatus
            {
                Id = taskStatus.Id,
                Status = TaskStatusType.Verified.ToString(),
                AutomationSystemStatus = taskStatus.AutomationSystemStatus,
                StartedAt = taskStatus.StartedAt,
                FinishedAt = taskStatus.FinishedAt,
                Mark = 2,
            }; 
            
            await taskStatusesRepository.Update(updateTaskStatus);
        }
    }
    
    private async Task SendToRecommendedAsync(CancellationToken cancellationToken)
    {
        using var scope = _serviceProvider.CreateScope();
        var taskStatusesRepository = scope.ServiceProvider.GetService<ITaskStatusesRepository>()!;
        var taskStatuses = await taskStatusesRepository.Get();

        var status = taskStatuses
            .FirstOrDefault(x => x.Status.Equals(TaskStatusType.Verified.ToString())
            && x.Mark < 3);

        if (status == null)
            return;
        
        var updateTaskStatus = new TaskStatus
        {
            Id = status.Id,
            Status = TaskStatusType.Recommended.ToString(),
            AutomationSystemStatus = status.AutomationSystemStatus,
            StartedAt = status.StartedAt,
            FinishedAt = status.FinishedAt,
            Mark = status.Mark,
        }; 
        
        await taskStatusesRepository.Update(updateTaskStatus);
    }
    
    private async Task SendToAiAsync(CancellationToken cancellationToken)
    {
        using var scope = _serviceProvider.CreateScope();
        var taskStatusesRepository = scope.ServiceProvider.GetService<ITaskStatusesRepository>()!;
        var minIoFileService = scope.ServiceProvider.GetService<IMinIoFileService>()!;
        var taskStatuses = await taskStatusesRepository.Get();

        // var status = taskStatuses
        //     .FirstOrDefault(x => x.Status.Equals(TaskStatusType.SendToCheck.ToString()));
        //
        // if (status == null)
        //     return;

        var files = new List<(MemoryStream, string)>();
        var keys = new List<string>()
        {
            "85b43091-0da2-4693-aad7-795dcb269633.jpg",
            "ab0c6781-82ff-4dc7-a864-5fdcae3fe9b8.jpg",
            "6425fbaf-0ac9-4b98-bcbe-8dc5120b9955.jpg",
            "6298e7e0-b361-4f6c-8287-6d16d70054d4.jpg",
            "a406df37-fe7f-451d-97e6-a623e652f686.jpg",
            "0c0e706e-1c5b-4c99-aa62-f93a47810546.jpg"
        };
        foreach (var foto in keys)
        {
            files.Add(await minIoFileService.Download(foto));
        }
        
        // foreach (var foto in status.Fotos)
        // {
        //     files.Add(await minIoFileService.Download(foto.Key));
        // }
        
        var result = await PostAsync(files);

        // var updateTaskStatus = new TaskStatus
        // {
        //     Id = status.Id,
        //     Status = TaskStatusType.AiVerified.ToString(),
        //     AutomationSystemStatus = AutomationSystemStatus.Complete.ToString(),
        //     StartedAt = status.StartedAt,
        //     FinishedAt = status.FinishedAt,
        //     Mark = 5,
        // }; 
        //
        // await taskStatusesRepository.Update(updateTaskStatus);
    }

    private async Task<string> PostAsync(List<(MemoryStream, string)> files)
    {
        using var client = new HttpClient { BaseAddress = new Uri(_aiOptions.Uri) };
        using var form = new MultipartFormDataContent();
        
        foreach (var file in files)
        {
            using var fileContent = new StreamContent(file.Item1);
            fileContent.Headers.ContentType = MediaTypeHeaderValue.Parse("multipart/form-data");

            form.Add(fileContent, "Key", file.Item2);
        }
        
        var response = await client.PostAsync("/check", form);
        response.EnsureSuccessStatusCode();

        return await response.Content.ReadAsStringAsync();
    }

}