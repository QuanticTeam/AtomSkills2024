using TaskStatus = backend.Core.Models.TaskStatus;

namespace backend.Core.Abstractions;

public interface ITaskStatusesRepository
{
    Task<List<TaskStatus>> Get();
    Task<int> Create(TaskStatus taskStatus);
    Task<int> Update(TaskStatus taskStatus);
}