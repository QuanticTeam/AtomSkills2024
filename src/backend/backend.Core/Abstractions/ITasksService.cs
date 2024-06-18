using backend.Core.Models;
using Task = backend.Core.Models.Task;
using TaskStatus = backend.Core.Models.TaskStatus;

namespace backend.Core.Abstractions;

public interface ITasksService
{
    Task<Task> Get();
    Task<int> TakeTaskInWork(string taskCode, string userKey);
    Task<List<TaskStatus>> GetTaskStatuses(string taskCode, string userKey);
    Task<int> SendTaskToCheck(int taskStatusId, List<Foto> fotos);
    Task<int> VerifiedTask(List<Defect> defects, int taskStatusId, int mark);
    Task<int> RecommendedRework(int taskId);
    Task<TaskStatus> GetTaskStatus(int id);
}