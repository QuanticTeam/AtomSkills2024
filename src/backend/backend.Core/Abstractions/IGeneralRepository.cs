using backend.Core.JsonModels;
using backend.Core.Models;
using Task = backend.Core.Models.Task;

namespace backend.Core.Abstractions;

public interface IGeneralRepository
{
    Task<IEnumerable<Trait>> GetTraits() => await _dbContext.Traits.ToListAsync();
    Task<IEnumerable<Task>> GetTasks() => await _dbContext.Tasks.ToListAsync();


    Task<int> UploadTrait(JsonTrait trait);
    Task<int> UploadTask(Task task);
    Task<int> UploadLesson(Lesson lesson);

}