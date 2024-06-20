using backend.Core.Abstractions;
using backend.Core.Models;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Task = System.Threading.Tasks.Task;
using TaskStatus = backend.Core.Models.TaskStatus;

namespace backend.Application.Services;

public class BackgroundTaskStatusService : BackgroundService
{
    private readonly IServiceProvider _serviceProvider;

    public BackgroundTaskStatusService(IServiceProvider serviceProvider)
    {
        _serviceProvider = serviceProvider;
        Thread.Sleep(TimeSpan.FromSeconds(15));
    }

    protected override async Task ExecuteAsync(CancellationToken cancellationToken) 
    { 
        while (!cancellationToken.IsCancellationRequested) 
        { 
            await SendExpiredAsync(cancellationToken);
            await SendToRecommendedAsync(cancellationToken);
            await Task.Delay(TimeSpan.FromSeconds(3), cancellationToken).ConfigureAwait(false);
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

        foreach (var taskStatus in taskStatuses)
        {
            if (taskStatus.Status.Equals(TaskStatusType.Verified.ToString())
                && taskStatus.Mark < 3)
            {
                var updateTaskStatus = new TaskStatus
                {
                    Id = taskStatus.Id,
                    Status = TaskStatusType.Recommended.ToString(),
                    AutomationSystemStatus = taskStatus.AutomationSystemStatus,
                    StartedAt = taskStatus.StartedAt,
                    FinishedAt = taskStatus.FinishedAt,
                    Mark = taskStatus.Mark,
                };

                await taskStatusesRepository.Update(updateTaskStatus);
            }
        }
    }
}