using backend.Core.JsonModels;
using backend.Core.Models;
using Task = backend.Core.Models.Task;

namespace backend.Core.Abstractions;

public interface IGeneralRepository
{
    Task<int> UploadTrait(JsonTrait trait);
    Task<int> UploadTask(Task task);
    Task<int> UploadLesson(Lesson lesson);
    Task<int> UploadTopic(Topic topic);
}