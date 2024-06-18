using backend.Core.Abstractions;
using backend.Core.Models;
using Task = backend.Core.Models.Task;
using TaskStatus = backend.Core.Models.TaskStatus;

namespace backend.Application.Services;

public class TasksService : ITasksService
{
    private readonly ITasksRepository _tasksRepository;
    private readonly ITaskStatusesRepository _taskStatusesRepository;
    private readonly IRecommendationsRepository _recommendationsRepository;
    private readonly IDefectsRepository _defectsRepository;
    private readonly IUsersRepository _usersRepository;
    private readonly IFotosRepository _fotosRepository;

    public TasksService(
        ITasksRepository tasksRepository,
        ITaskStatusesRepository taskStatusesRepository,
        IRecommendationsRepository recommendationsRepository,
        IDefectsRepository defectsRepository,
        IUsersRepository usersRepository,
        IFotosRepository fotosRepository)
    {
        _tasksRepository = tasksRepository;
        _taskStatusesRepository = taskStatusesRepository;
        _recommendationsRepository = recommendationsRepository;
        _defectsRepository = defectsRepository;
        _usersRepository = usersRepository;
        _fotosRepository = fotosRepository;
    }

    public async Task<Task> Get()
    {
        var tasks = await _tasksRepository.Get();
        return tasks.First();
    }
    
    public async Task<List<TaskStatus>> GetTaskStatuses(string taskCode, string userKey)
    {
        var tasks = await _taskStatusesRepository.Get();

        if (!string.IsNullOrEmpty(userKey))
            tasks = tasks.Where(x => x.UserKey.Equals(userKey)).ToList();
        
        return tasks
            .Where(x => x.TaskCode.Equals(taskCode))
            .ToList();
    }
    
    public async Task<TaskStatus> GetTaskStatus(int id)
    {
        var tasks = await _taskStatusesRepository.Get();

        return tasks.FirstOrDefault(x => x.Id.Equals(id))!;
    }
    
    public async Task<int> TakeTaskInWork(string taskCode, string userKey)
    {
        var users = await _usersRepository.Get();
        var user = users.FirstOrDefault(x => x.Key.ToString().Equals(userKey));
        var tasks = await _tasksRepository.Get();
        var task = tasks.FirstOrDefault(x => x.Code.Equals(taskCode));

        if (user == null || task == null)
            return 0;

        var taskStatus = new TaskStatus
        {
            Status = TaskStatusType.InWork.ToString(),
            AutomationSystemStatus = AutomationSystemStatus.None.ToString(),
            StartedAt = DateTime.UtcNow,
            FinishedAt = null,
            Mark = null,
            Fotos = [],
            UserKey = user.Key.ToString(),
            TaskCode = task.Code,
            Recommendations = [],
            Defects = [],
        };

        return await _taskStatusesRepository.Create(taskStatus);
    }
    
    public async Task<int> SendTaskToCheck(int taskStatusId, List<Foto> fotos)
    {
        var taskStatuses = await _taskStatusesRepository.Get();
        var taskStatus = taskStatuses.FirstOrDefault(x => x.Id.Equals(taskStatusId));
        
        if (taskStatus == null)
            return 0;
        
        if (!taskStatus.Status.Equals(TaskStatusType.InWork.ToString()))
            return 0;

        var editTaskStatus = new TaskStatus
        {
            Id = taskStatus.Id,
            Status = TaskStatusType.SendToCheck.ToString(),
            AutomationSystemStatus = taskStatus.AutomationSystemStatus,
            StartedAt = taskStatus.StartedAt,
            FinishedAt = DateTime.UtcNow,
            Mark = taskStatus.Mark,
            Fotos = fotos,
            UserKey = taskStatus.UserKey,
            TaskCode = taskStatus.TaskCode,
        };
        
        var result = await _taskStatusesRepository.Update(editTaskStatus);
        var resultFotos = await _fotosRepository.Create(fotos, editTaskStatus.Id);

        return result + resultFotos;
    }

    public async Task<int> VerifiedTask(List<Defect> defects, int taskStatusId, int mark)
    {
        var taskStatuses = await _taskStatusesRepository.Get();
        var taskStatus = taskStatuses.FirstOrDefault(x => x.Id.Equals(taskStatusId));
        
        if (taskStatus == null)
            return 0;
        
        if (!taskStatus.Status.Equals(TaskStatusType.SendToCheck.ToString())
            &&
            !taskStatus.Status.Equals(TaskStatusType.AiVerified.ToString()))
            return 0;
        
        foreach (var defect in defects)
        {
            await _defectsRepository.Create(defect);
        }
        
        var editTaskStatus = new TaskStatus
        {
            Id = taskStatus.Id,
            Status = TaskStatusType.Verified.ToString(),
            AutomationSystemStatus = taskStatus.AutomationSystemStatus,
            StartedAt = taskStatus.StartedAt,
            FinishedAt = taskStatus.FinishedAt,
            Mark = mark,
            UserKey = taskStatus.UserKey,
            TaskCode = taskStatus.TaskCode,
        };
        
        return await _taskStatusesRepository.Update(editTaskStatus);
    }
    
    public async Task<int> RecommendedRework(int taskStatusId)
    {
        var taskStatuses = await _taskStatusesRepository.Get();
        var taskStatus = taskStatuses.FirstOrDefault(x => x.Id.Equals(taskStatusId));

        if (taskStatus == null)
            return 0;

        if (taskStatus.Status.Equals(TaskStatusType.Recommended.ToString()))
            return 1;

        if (!taskStatus.Status.Equals(TaskStatusType.Verified.ToString()))
            return 0;
        
        var editTaskStatus = new TaskStatus
        {
            Id = taskStatus.Id,
            Status = TaskStatusType.Recommended.ToString(),
            AutomationSystemStatus = taskStatus.AutomationSystemStatus,
            StartedAt = taskStatus.StartedAt,
            FinishedAt = taskStatus.FinishedAt,
            Mark = taskStatus.Mark,
            UserKey = taskStatus.UserKey,
            TaskCode = taskStatus.TaskCode,
        };
        
        return await _taskStatusesRepository.Update(editTaskStatus);
    }
}