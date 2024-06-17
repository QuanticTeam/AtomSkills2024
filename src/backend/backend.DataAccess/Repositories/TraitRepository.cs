using backend.Core.Abstractions;
using backend.Core.Models;
using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;

namespace backend.DataAccess.Repositories;

public class TraitRepository : ITraitRepository
{
    private readonly BackendDbContext _dbContext;
    
    public TraitRepository(BackendDbContext dbContext)
    {
        _dbContext = dbContext;
    }
    
    public async Task<List<Trait>> Get()
    {
        var traitRecords = await _dbContext.Traits
            .Include(x => x.LessonRecords)
            .Include(x => x.TopicRecords)
            .AsNoTracking()
            .ToListAsync();

        var traits = traitRecords
            .Select(x => new Trait
            {
                Code = x.Code,
                Name = x.Name,
                Description = x.Description,
                TopicRecords = x.TopicRecords.Select(t => new Topic
                {
                    Code = t.Code,
                    Title = t.Title,
                    Description = t.Description,
                }).ToList(),
                LessonRecords = x.LessonRecords.Select(l => new Lesson
                {
                    Code = l.Code,
                    Title = l.Title,
                    Content = l.Content,
                    Author = l.Author,
                    Supplements = l.SupplementKeys.ToList(),
                }).ToList(),
            }).ToList();
        
        return traits;
    }

    public async Task<int> Create(Trait trait)
    {
        var traitRecord = new TraitRecord
        {
            Code = trait.Code,
            Name = trait.Name,
            Description = trait.Description,
        };

        await _dbContext.Traits.AddAsync(traitRecord);
        return await _dbContext.SaveChangesAsync();
    }

    public async Task<int> Update(Trait trait)
    {
        return await _dbContext.Traits
            .Where(x => x.Code.Equals(trait.Code))
            .ExecuteUpdateAsync(x => x
                .SetProperty(b => b.Name, b => trait.Name)
                .SetProperty(b => b.Description, b => trait.Description));
    }

    public async Task<int> Delete(string code)
    {
        return await _dbContext.Traits
            .Where(x => x.Code.Equals(code))
            .ExecuteDeleteAsync();
    }
}