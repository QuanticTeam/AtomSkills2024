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

        foreach (var task in _contentLoadService.LoadTasks())
        {
            foreach (var supplement in task.Supplement)
            {
                var s = await _minIoFileService.Upload(supplement);
            }

            // var files = _minIoFileService.Upload(task.Supplement);
            // await _repository.UploadTask(new Task
            // {

            // });
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