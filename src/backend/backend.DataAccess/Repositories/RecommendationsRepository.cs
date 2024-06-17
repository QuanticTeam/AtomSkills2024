using backend.Core.Abstractions;
using backend.Core.Models;
using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;

namespace backend.DataAccess.Repositories;

public class RecommendationsRepository : IRecommendationsRepository
{
    private readonly BackendDbContext _dbContext;
    
    public RecommendationsRepository(BackendDbContext dbContext)
    {
        _dbContext = dbContext;
    }
    
    public async Task<List<Recommendation>> Get()
    {
        var recommendationRecords = await _dbContext.Recommendations
            .AsNoTracking()
            .ToListAsync();

        var recommendations = recommendationRecords
            .Select(x => new Recommendation
            {
                Text = x.Text,
                FileKeys = x.FileKeys,
            }).ToList();
        
        return recommendations;
    }

    public async Task<int> Create(Recommendation recommendation)
    {
        var taskStatusRecord = await _dbContext.TaskStatuses.FindAsync(recommendation.TaskStatusRecordId);

        if (taskStatusRecord == null)
            return 0;
        
        var recommendationRecord = new RecommendationRecord
        {
            Text = recommendation.Text,
            FileKeys = recommendation.FileKeys,
            TaskStatusRecordId = taskStatusRecord!.Id,
            TaskStatusRecord = taskStatusRecord
        };

        await _dbContext.Recommendations.AddAsync(recommendationRecord);
        return await _dbContext.SaveChangesAsync();
    }
}