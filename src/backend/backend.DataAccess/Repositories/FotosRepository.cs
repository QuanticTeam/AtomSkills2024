using backend.Core.Abstractions;
using backend.Core.Models;
using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;

namespace backend.DataAccess.Repositories;

public class FotosRepository : IFotosRepository
{
    private readonly BackendDbContext _dbContext;
    
    public FotosRepository(BackendDbContext dbContext)
    {
        _dbContext = dbContext;
    }
    
    public async Task<List<Foto>> Get()
    {
        var fotoRecords = await _dbContext.Fotos
            .AsNoTracking()
            .ToListAsync();

        var fotos = fotoRecords
            .Select(x => new Foto
            {
                Comment = x.Comment,
                Key = x.FotoKey,
            }).ToList();
        
        return fotos;
    }
    
    public async Task<int> Create(List<Foto> fotos, int statusId)
    {
        var statusRecord = await _dbContext.TaskStatuses.FindAsync(statusId);
        
        if (statusRecord == null)
            return 0;
        
        var fotoRecords = fotos.Select(x => new FotoRecord()
        {
            Comment = x.Comment,
            FotoKey = x.Key,
            TaskStatusRecordId = statusRecord.Id,
            TaskStatusRecord = statusRecord,
        });

        await _dbContext.Fotos.AddRangeAsync(fotoRecords);
        return await _dbContext.SaveChangesAsync();
    }
}