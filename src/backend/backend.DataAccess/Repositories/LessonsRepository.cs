using backend.Core.Abstractions;
using backend.Core.Models;
using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;
using Task = backend.Core.Models.Task;

namespace backend.DataAccess.Repositories;

public class LessonsRepository : ILessonsRepository
{
    private readonly BackendDbContext _dbContext;
    
    public LessonsRepository(BackendDbContext dbContext)
    {
        _dbContext = dbContext;
    }
    
    public async Task<List<Lesson>> Get()
    {
        var lessonRecords = await _dbContext.Lessons
            .Include(x => x.TraitRecords)
            .Include(x => x.TaskRecords)
            .AsNoTracking()
            .ToListAsync();

        var lessons = lessonRecords
            .Select(x => new Lesson
            {
                Content = x.Content,
                Code = x.Code,
                Author = x.Author,
                Title = x.Title,
                Supplements = x.SupplementKeys.ToList(),
                Traits = x.TraitRecords.Select(t => new Trait
                {
                    Code = t.Code,
                    Name = t.Name,
                    Description = t.Description,
                }).ToList(),
                Tasks = x.TaskRecords.Select(t => new Task
                {
                    Code = t.Code,
                    Title = t.Title,
                    Content = t.Content,
                    Difficulty = t.Difficult,
                    Time = t.Time,
                    Supplements = t.SupplementKeys.ToList(),
                }).ToList(),
            }).ToList();
        
        return lessons;
    }

    public async Task<int> Create(Lesson lesson)
    {
        var somethingRecord = new LessonRecord
        {
            Code = lesson.Code,
            Title = lesson.Title,
            Content = lesson.Content,
            Author = lesson.Author,
            SupplementKeys = lesson.Supplements.ToArray(),
        };

        await _dbContext.Lessons.AddAsync(somethingRecord);
        return await _dbContext.SaveChangesAsync();
    }

    public async Task<int> Update(Lesson lesson)
    {
        return await _dbContext.Lessons
            .Where(x => x.Code.Equals(lesson.Code))
            .ExecuteUpdateAsync(x => x
                .SetProperty(b => b.Title, b => lesson.Title)
                .SetProperty(b => b.Content, b => lesson.Content)
                .SetProperty(b => b.Author, b => lesson.Author)
                .SetProperty(b => b.SupplementKeys, b => lesson.Supplements.ToArray()));
    }

    public async Task<int> Delete(string code)
    {
        return await _dbContext.Lessons
            .Where(x => x.Code.Equals(code))
            .ExecuteDeleteAsync();
    }
}