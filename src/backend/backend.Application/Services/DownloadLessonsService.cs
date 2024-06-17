using backend.Core.Abstractions;
using backend.Core.Models;
using Task = backend.Core.Models.Task;

namespace backend.Application.Services;

public class DownloadLessonsService : IDownloadService
{
    private readonly IGeneralRepository _repository;
    private readonly IMinIoFileService _minIoFileService;
    private readonly IContentLoadService _contentLoadService;

    public DownloadLessonsService(
        IGeneralRepository repository,
        IMinIoFileService minIoFileService,
        IContentLoadService contentLoadService)
    {
        _repository = repository;
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