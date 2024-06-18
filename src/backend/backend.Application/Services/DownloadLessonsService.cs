using backend.Core.Abstractions;
using backend.Core.Models;
using Task = backend.Core.Models.Task;

namespace backend.Application.Services;

public class DownloadLessonsService : IDownloadService
{
    private readonly IGeneralRepository _repository;
    private readonly ITasksRepository _tasksRepository;
    private readonly IMinIoFileService _minIoFileService;
    private readonly IContentLoadService _contentLoadService;

    public DownloadLessonsService(
        IGeneralRepository repository,
        ITasksRepository tasksRepository,
        IMinIoFileService minIoFileService,
        IContentLoadService contentLoadService)
    {
        _repository = repository;
        _tasksRepository = tasksRepository;
        _minIoFileService = minIoFileService;
        _contentLoadService = contentLoadService;
    }

    public async Task<int> Download()
    {
        foreach (var trait in _contentLoadService.LoadTraits())
        {
            await _repository.UploadTrait(trait);
        }

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
                Tasks = jsonLesson.Tasks,
                Traits = jsonLesson.Traits, 
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


        // List<Trait> traits = _contentLoadService.LoadTraits().Select(trait => new Trait
        // {
        //     Code = trait.Code,

        // });
        var tasks = new List<Task>();
        var lessons = new List<Lesson>();
        var topics = new List<Topic>();
        // return await _repository.Download(traits, tasks, lessons, topics);
        return 100500;
    }
}