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

    public async Task<int> UploadTrait(JsonTrait trait)
    {
        await _dbContext.Traits.AddAsync(new TraitRecord
        {
            Code = trait.Code,
            Name = trait.Name,
            Description = trait.Description,
        });
        return await _dbContext.SaveChangesAsync();
    }

    public async Task<int> UploadTask(Task task)
    {
        await _dbContext.Tasks.AddAsync(new TaskRecord
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

    public async Task<int> UploadLesson(Lesson lesson)
    {
        var traitRecords = await _dbContext.Traits.ToListAsync();
        var taskRecords = await _dbContext.Tasks.ToListAsync();

        await _dbContext.Lessons.AddAsync(
            new LessonRecord
            {
                Code = lesson.Code,
                Title = lesson.Title,
                Content = lesson.Content,
                Author = lesson.Author,
                SupplementKeys = lesson.Supplements.ToArray(),
                TraitRecords = traitRecords
                    .Where(t => lesson.Traits.Exists(trait => t.Code.Equals(trait.Code)))
                    .ToList(),
                TaskRecords = taskRecords
                    .Where(t => lesson.Tasks.Exists(task => t.Code.Equals(task.Code)))
                    .ToList(),
            });

        return await _dbContext.SaveChangesAsync();
    }
    
    public async Task<int> UploadTopic(Topic topic)
    {
        var traitRecords = await _dbContext.Traits.ToListAsync();
        var lessonRecords = await _dbContext.Lessons.ToListAsync();

        await _dbContext.Topics.AddAsync(new TopicRecord
        {
            Code = topic.Code,
            Title = topic.Title,
            Description = topic.Description,
            TraitRecords = traitRecords
                .Where(t => topic.Traits.Exists(trait => t.Code.Equals(trait.Code)))
                .ToList(),
            LessonRecords = lessonRecords
                .Where(l => topic.Lessons.Exists(task => l.Code.Equals(task.Code)))
                .ToList(),
        });

        return await _dbContext.SaveChangesAsync();
    }
}