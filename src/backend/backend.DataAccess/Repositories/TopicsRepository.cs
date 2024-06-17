using backend.Core.Abstractions;
using backend.Core.Models;
using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;

namespace backend.DataAccess.Repositories;

public class TopicsRepository : ITopicsRepository
{
    private readonly BackendDbContext _dbContext;
    
    public TopicsRepository(BackendDbContext dbContext)
    {
        _dbContext = dbContext;
    }
    
    public async Task<List<Topic>> Get()
    {
        var topicRecords = await _dbContext.Topics
            .Include(x => x.LessonRecords)
            .Include(x => x.TraitRecords)
            .AsNoTracking()
            .ToListAsync();

        var topics = topicRecords
            .Select(x => new Topic
            {
                Description = x.Description,
                Code = x.Code,
                Title = x.Title,
                Lessons = x.LessonRecords.Select(l => new Lesson
                {
                    Code = l.Code,
                    Title = l.Title,
                    Content = l.Content,
                    Author = l.Author,
                    Supplements = l.SupplementKeys.ToList(),
                }).ToList(),
                Traits = x.TraitRecords.Select(t => new Trait
                {
                    Code = t.Code,
                    Name = t.Name,
                    Description = t.Description,
                }).ToList(),
            }).ToList();
        
        return topics;
    }

    public async Task<int> Create(Topic topic)
    {
        var topicRecord = new TopicRecord
        {
            Code = topic.Code,
            Title = topic.Title,
            Description = topic.Description,
        };

        await _dbContext.Topics.AddAsync(topicRecord);
        return await _dbContext.SaveChangesAsync();
    }

    public async Task<int> Update(Topic topic)
    {
        return await _dbContext.Topics
            .Where(x => x.Code.Equals(topic.Code))
            .ExecuteUpdateAsync(x => x
                .SetProperty(b => b.Title, b => topic.Title)
                .SetProperty(b => b.Description, b => topic.Description));
    }

    public async Task<int> Delete(string code)
    {
        return await _dbContext.Topics
            .Where(x => x.Code.Equals(code))
            .ExecuteDeleteAsync();
    }
}