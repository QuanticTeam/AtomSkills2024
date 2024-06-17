using backend.Core.Abstractions;
using backend.Core.JsonModels;
using backend.Core.Models;
using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;
using Task = backend.Core.Models.Task;

namespace backend.DataAccess.Repositories;

public class GeneralRepository : IGeneralRepository
{
    private readonly BackendDbContext _dbContext;
    
    public GeneralRepository(BackendDbContext dbContext)
    {
        _dbContext = dbContext;
    }

    public async Task<int> Download(
        List<Trait> traits, 
        List<Task> tasks, 
        List<Lesson> lessons,
        List<Topic> topics)
    {
        // var traitResult = await DownloadTrait(traits);
        
        // var taskResult = await DownloadTasks(tasks);
        
        var lessonResult = await DownloadLessons(lessons);
        
        var topicResult = await DownloadTopics(topics);
        
        return 100500; //traitResult + taskResult + lessonResult + topicResult;
    }
    
    public async Task<int> UploadTrait(JsonTrait trait)
    {
        await _dbContext.Traits.AddRangeAsync(new TraitRecord
        {
            Code = trait.Code,
            Name = trait.Name,
            Description = trait.Description,
        });
        return await _dbContext.SaveChangesAsync();
    }

    public async Task<int> UploadTask(Task task)
    {
        await _dbContext.Tasks.AddRangeAsync(new TaskRecord
        {
            Code = task.Code,
            Title = task.Title,
            Content = task.Content,
            Difficult = task.Difficulty,
            SupplementKeys = task.Supplements.ToArray(),
            Time = task.Time,
        });
        return await _dbContext.SaveChangesAsync();
    }
    
    private async Task<int> DownloadLessons(List<Lesson> lessons)
    {
        var traitRecords = await _dbContext.Traits.ToListAsync();
        
        var taskRecords = await _dbContext.Tasks.ToListAsync();
        
        var lessonRecords = lessons.Select(x => new LessonRecord
        {
            Code = x.Code,
            Title = x.Title,
            Content = x.Content,
            Author = x.Author,
            SupplementKeys = x.Supplements.ToArray(),
            TraitRecords = traitRecords
                .Where(t => x.Traits.Exists(trait => t.Code.Equals(trait.Code)))
                .ToList(),
            TaskRecords = taskRecords
                .Where(t => x.Tasks.Exists(task => t.Code.Equals(task.Code)))
                .ToList(),
        });
        
        await _dbContext.Lessons.AddRangeAsync(lessonRecords);
        return await _dbContext.SaveChangesAsync();
    }
    
    private async Task<int> DownloadTopics(List<Topic> topics)
    {
        var traitRecords = await _dbContext.Traits.ToListAsync();
        
        var lessonRecords = await _dbContext.Lessons.ToListAsync();
        
        var topicRecords = topics.Select(x => new TopicRecord
        {
            Code = x.Code,
            Title = x.Title,
            Description = x.Description,
            TraitRecords = traitRecords
                .Where(t => x.Traits.Exists(trait => t.Code.Equals(trait.Code)))
                .ToList(),
            LessonRecords = lessonRecords
                .Where(l => x.Lessons.Exists(task => l.Code.Equals(task.Code)))
                .ToList(),
        });
        
        await _dbContext.Topics.AddRangeAsync(topicRecords);
        return await _dbContext.SaveChangesAsync();
    }
}