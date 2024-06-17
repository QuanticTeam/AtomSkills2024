using backend.Core.JsonModels;
using backend.Core.Models;
using Task = backend.Core.Models.Task;

namespace backend.Core.Abstractions;

public interface IGeneralRepository
{
    Task<int> Download(
        List<Trait> traits, 
        List<Task> tasks, 
        List<Lesson> lessons,
        List<Topic> topics);

    Task<int> UploadTrait(JsonTrait trait);
    Task<int> UploadTask(Task task);
}