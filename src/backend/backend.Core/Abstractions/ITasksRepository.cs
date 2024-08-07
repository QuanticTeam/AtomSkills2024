using Task = backend.Core.Models.Task;

namespace backend.Core.Abstractions;

public interface ITasksRepository
{
    Task<Task?> GetByCode(string taskCode);
    Task<List<Task>> Get();
    Task<int> Create(Task task);
    Task<int> Update(Task task);
    Task<int> Delete(string code);
}