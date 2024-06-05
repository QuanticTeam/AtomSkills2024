using backend.Core.Abstractions;
using backend.Core.Models;
using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;

namespace backend.DataAccess.Repositories;

public class SomeFilesRepository : ISomeFilesRepository
{
    private readonly BackendDbContext _dbContext;
    
    public SomeFilesRepository(BackendDbContext dbContext)
    {
        _dbContext = dbContext;
    }

    public async Task<List<SomeFile>> Get()
    {
        var someFileRecords = await _dbContext.SomeFiles
            .AsNoTracking()
            .ToListAsync();

        var someFiles = someFileRecords
            .Select(x => new SomeFile
            {
                Key = x.Key,
                Name = x.Name,
                ContentType = x.ContentType,
                Content = x.Content,
            })
            .ToList();
        
        return someFiles;
    }

    public async Task<int> Create(SomeFile someFile)
    {
        var someFileRecord = new SomeFileRecord
        {
            Key = someFile.Key,
            Name = someFile.Name,
            ContentType = someFile.ContentType,
            Content = someFile.Content,
        };

        await _dbContext.SomeFiles.AddAsync(someFileRecord);
        return await _dbContext.SaveChangesAsync();
    }

    public async Task<int> Update(SomeFile someFile)
    {
        return await _dbContext.SomeFiles
            .Where(x => x.Key.Equals(someFile.Key))
            .ExecuteUpdateAsync(x => x
                .SetProperty(b => b.Name, b => someFile.Name)
                .SetProperty(b => b.ContentType, b => someFile.ContentType)
                .SetProperty(b => b.Content, b => someFile.Content));
    }

    public async Task<int> Delete(Guid key)
    {
        return await _dbContext.SomeFiles
            .Where(x => x.Key.Equals(key))
            .ExecuteDeleteAsync();
    }
}