using backend.Core.JsonModels;

namespace backend.Core.Abstractions;

public interface IContentLoadService
{
    IEnumerable<JsonTrait> LoadTraits();
    IEnumerable<JsonTopic> LoadTopics();
    IEnumerable<JsonLesson> LoadLessons();
    IEnumerable<JsonTask> LoadTasks();
    Dictionary<string, string> Dictionary { get; }
}