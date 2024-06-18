using backend.Core.Abstractions;
using backend.Core.Models;
using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;

namespace backend.DataAccess.Repositories;

public class DefectsRepository : IDefectsRepository
{
    private readonly BackendDbContext _dbContext;
    
    public DefectsRepository(BackendDbContext dbContext)
    {
        _dbContext = dbContext;
    }
    
    public async Task<List<Defect>> Get()
    {
        var defectRecords = await _dbContext.Defects
            .AsNoTracking()
            .ToListAsync();

        var defects = defectRecords
            .Select(x => new Defect
            {
                Id = x.Id,
                FileKey = x.FileKey,
                Codes = x.Codes,
                Comment = x.Comment,
                X1 = x.X1,
                Y1 = x.Y1,
                X2 = x.X2,
                Y2 = x.Y2,
            }).ToList();
        
        return defects;
    }
    
    public async Task<int> Create(Defect defect)
    {
        var statusRecord = await _dbContext.TaskStatuses.FindAsync(defect.TaskStatusRecordId);
        
        if (statusRecord == null)
            return 0;
        
        var defectRecord = new DefectRecord()
        {
            FileKey = defect.FileKey,
            Codes = defect.Codes,
            Comment = defect.Comment,
            X1 = defect.X1,
            Y1 = defect.Y1,
            X2 = defect.X2,
            Y2 = defect.Y2,
            TaskStatusRecordId = statusRecord.Id,
            TaskStatusRecord = statusRecord,
        };

        await _dbContext.Defects.AddAsync(defectRecord);
        return await _dbContext.SaveChangesAsync();
    }
}