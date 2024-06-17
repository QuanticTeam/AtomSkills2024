using backend.Core.Abstractions;
using backend.Core.Models;
using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;
using TaskStatus = backend.Core.Models.TaskStatus;

namespace backend.DataAccess.Repositories;

public class TaskStatusesRepository : ITaskStatusesRepository
{
    private readonly BackendDbContext _dbContext;
    
    public TaskStatusesRepository(BackendDbContext dbContext)
    {
        _dbContext = dbContext;
    }
    
    public async Task<List<TaskStatus>> Get()
    {
        var taskStatusRecords = await _dbContext.TaskStatuses
            .Include(x => x.UserRecord)
            .Include(x => x.RecommendationRecords)
            .Include(x => x.TaskRecord)
            .AsNoTracking()
            .ToListAsync();

        var taskStatusList = taskStatusRecords
            .Select(x => new TaskStatus
            {
                Id = x.Id,
                Status = x.Status,
                AutomationSystemStatus = x.AutomationSystemStatus,
                StartedAt = x.StartedAt,
                FinishedAt = x.FinishedAt,
                Mark = x.Mark,
                FotoKeys = x.FotoKeys,
                UserKey = x.UserRecord?.Key.ToString() ?? string.Empty,
                TaskId = x.TaskRecord!.Id,
                Recommendations = x.RecommendationRecords.Select(r => new Recommendation
                {
                    Text = r.Text,
                    FileKeys = r.FileKeys,
                }).ToList(),
            }).ToList();
        
        return taskStatusList;
    }

    public async Task<int> Create(TaskStatus taskStatus)
    {
        var userRecord = _dbContext.Users.FirstOrDefault(x => x.Key.Equals(taskStatus.UserKey));
        var taskRecord = await _dbContext.Tasks.FindAsync(taskStatus.TaskId);

        if (userRecord == null || taskRecord == null)
            return 0;
        
        var taskStatusRecord = new TaskStatusRecord
        {
            Status = taskStatus.Status,
            AutomationSystemStatus = taskStatus.AutomationSystemStatus,
            StartedAt = taskStatus.StartedAt,
            FinishedAt = taskStatus.FinishedAt,
            Mark = taskStatus.Mark,
            FotoKeys = taskStatus.FotoKeys,
            UserRecordId = userRecord.Id,
            UserRecord = userRecord,
            TaskRecordId = taskRecord.Id,
            TaskRecord = taskRecord,
            RecommendationRecords = new List<RecommendationRecord>()
        };

        await _dbContext.TaskStatuses.AddAsync(taskStatusRecord);
        return await _dbContext.SaveChangesAsync();
    }
    
    public async Task<int> Update(TaskStatus taskStatus)
    {
        return await _dbContext.TaskStatuses
            .Where(x => x.Id.Equals(taskStatus.Id))
            .ExecuteUpdateAsync(x => x
                .SetProperty(b => b.Status, b => taskStatus.Status)
                .SetProperty(b => b.AutomationSystemStatus, b => taskStatus.AutomationSystemStatus)
                .SetProperty(b => b.StartedAt, b => taskStatus.StartedAt)
                .SetProperty(b => b.FinishedAt, b => taskStatus.FinishedAt)
                .SetProperty(b => b.Mark, b => taskStatus.Mark)
                .SetProperty(b => b.FotoKeys, b => taskStatus.FotoKeys));
    }
}