using backend.Core.Abstractions;
using Task = backend.Core.Models.Task;

namespace backend.Application.Services;

public class TasksService
{
    private readonly ITasksRepository _tasksRepository;
    private readonly ITaskStatusesRepository _taskStatusesRepository;
    private readonly IRecommendationsRepository _recommendationsRepository;

    public TasksService(
        ITasksRepository tasksRepository,
        ITaskStatusesRepository taskStatusesRepository,
        IRecommendationsRepository recommendationsRepository)
    {
        _tasksRepository = tasksRepository;
        _taskStatusesRepository = taskStatusesRepository;
        _recommendationsRepository = recommendationsRepository;
    }

    public async Task<Task> Get()
    {
        var tasks = await _tasksRepository.Get();
        return tasks.First();
    }
}