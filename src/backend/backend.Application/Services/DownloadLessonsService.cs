using backend.Core.Abstractions;
using backend.Core.Models;
using Task = backend.Core.Models.Task;

namespace backend.Application.Services;

public class DownloadLessonsService : IDownloadService
{
    private readonly IGeneralRepository _repository;
    private readonly ITraitRepository _traitRepository;
    private readonly ITasksRepository _tasksRepository;
    private readonly ILessonsRepository _lessonRepository;
    private readonly ITopicsRepository _topicRepository;
    private readonly IMinIoFileService _minIoFileService;
    private readonly IContentLoadService _contentLoadService;

    public DownloadLessonsService(
        IGeneralRepository repository,
        ITasksRepository tasksRepository,
        ITraitRepository traitRepository,
        ILessonsRepository lessonsRepository,
        ITopicsRepository topicsRepository,
        IMinIoFileService minIoFileService,
        IContentLoadService contentLoadService)
    {
        _repository = repository;
        _tasksRepository = tasksRepository;
        _traitRepository = traitRepository;
        _lessonRepository = lessonsRepository;
        _minIoFileService = minIoFileService;
        _contentLoadService = contentLoadService;
        _topicRepository = topicsRepository;
    }

    public async Task<int> Download()
    {
        foreach (var trait in _contentLoadService.LoadTraits())
        {
            await _repository.UploadTrait(trait);
        }

        var traits = await _traitRepository.Get();

        foreach (var jsonTask in _contentLoadService.LoadTasks())
        {
            var task = new Task
            {
                Code = jsonTask.Code,
                Title = jsonTask.Title,
                Content = jsonTask.Content,
                Supplements = new List<string> {},
                Difficulty = jsonTask.Difficulty,
                Time = jsonTask.Time,
            };

            foreach (var supplement in jsonTask.Supplement)
            {
                var s = await _minIoFileService.Upload(supplement);
                if (s != null)
                {
                    task.Supplements.Add(s.Key);
                }
            }

            await _repository.UploadTask(task);
        }

        var tasks = await _tasksRepository.Get();

        foreach (var jsonLesson in _contentLoadService.LoadLessons())
        {
            var lesson = new Lesson
            {
                Code = jsonLesson.Code,
                Title = jsonLesson.Title,
                Content = jsonLesson.Content,
                Author = jsonLesson.Author,
                Supplements = new List<string> {},
                Tasks = tasks.Where(t => jsonLesson.Tasks.Contains(t.Code)).ToList(),
                Traits = traits.Where(t => jsonLesson.Traits.Contains(t.Code)).ToList(),
            };

            foreach (var supplement in jsonLesson.Supplement)
            {
                var s = await _minIoFileService.Upload(supplement);
                if (s != null)
                {
                    lesson.Supplements.Add(s.Key);
                }
            }

            await _repository.UploadLesson(lesson);
        }

        var lessons = await _lessonRepository.Get();

        foreach (var jsonTopic in _contentLoadService.LoadTopics())
        {
            var topic = new Topic
            {
                Code = jsonTopic.Code,
                Title = jsonTopic.Title,
                Description = jsonTopic.Description,
                Lessons = lessons.Where(l => jsonTopic.Lessons.Contains(l.Code)).ToList(),
                Traits = traits.Where(t => jsonTopic.Traits.Contains(t.Code)).ToList(),
            };

            await _repository.UploadTopic(topic);
        }

        var counts = new List<int>
        {
            (await _traitRepository.Get()).Count,
            (await _topicRepository.Get()).Count,
            (await _lessonRepository.Get()).Count,
            (await _tasksRepository.Get()).Count,
        };

        return counts.Sum();
    }
}