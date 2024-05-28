using backend.Core.Abstractions;
using backend.Core.Models;
using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;

namespace backend.DataAccess.Repositories;

public class SomethingsRepository : ISomethingsRepository
{
    private readonly BackendDbContext _dbContext;
    
    public SomethingsRepository(BackendDbContext dbContext)
    {
        _dbContext = dbContext;
    }

    public async Task<List<Something>> Get()
    {
        var somethingRecords = await _dbContext.Somethings
            .AsNoTracking()
            .ToListAsync();

        var somethings = somethingRecords
            .Select(x => new Something(x.Key, x.Name, x.Number, x.Integer, x.DateTime, x.FileKey))
            .ToList();
        
        return somethings;
    }

    public async Task<int> Create(Something something)
    {
        var somethingRecord = new SomethingRecord()
        {
            Key = something.Key,
            Name = something.Name,
            Number = something.Number,
            Integer = something.Integer,
            DateTime = something.DateTime,
            FileKey = something.FileKey,
        };

        await _dbContext.Somethings.AddAsync(somethingRecord);
        return await _dbContext.SaveChangesAsync();
    }

    public async Task<int> Update(Something something)
    {
        return await _dbContext.Somethings
            .Where(x => x.Key.Equals(something.Key))
            .ExecuteUpdateAsync(x => x
                .SetProperty(b => b.Name, b => something.Name)
                .SetProperty(b => b.Number, b => something.Number)
                .SetProperty(b => b.Integer, b => something.Integer)
                .SetProperty(b => b.DateTime, b => something.DateTime)
                .SetProperty(b => b.FileKey, b => something.FileKey));
    }

    public async Task<int> Delete(Guid key)
    {
        return await _dbContext.Somethings
            .Where(x => x.Key.Equals(key))
            .ExecuteDeleteAsync();
    }
}