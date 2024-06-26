using backend.Core.Abstractions;
using backend.Core.Models;
using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;
using Task = backend.Core.Models.Task;
using TaskStatus = backend.Core.Models.TaskStatus;

namespace backend.DataAccess.Repositories;

public class TasksRepository : ITasksRepository
{
    private readonly BackendDbContext _dbContext;
    
    public TasksRepository(BackendDbContext dbContext)
    {
        _dbContext = dbContext;
    }

    public async Task<Task?> GetByCode(string taskCode)
    {
        var taskRecords = await _dbContext.Tasks
            .Include(x => x.LessonRecords)
            .Include(x => x.TaskStatusRecords)
            .ThenInclude(taskStatusRecord => taskStatusRecord.UserRecord)
            .Include(taskRecord => taskRecord.TaskStatusRecords).ThenInclude(taskStatusRecord => taskStatusRecord.Fotos)
            .Include(taskRecord => taskRecord.TaskStatusRecords)
            .ThenInclude(taskStatusRecord => taskStatusRecord.RecommendationRecords)
            .Include(taskRecord => taskRecord.TaskStatusRecords)
            .ThenInclude(taskStatusRecord => taskStatusRecord.DefectRecords)
            .Where(t => t.Code.Equals(taskCode))
            .AsNoTracking()
            .ToListAsync();

        var task = taskRecords.FirstOrDefault();

        if (task == null) return null;

        return new Task
            {
                Content = task.Content,
                Code = task.Code,
                Title = task.Title,
                Supplements = task.SupplementKeys.ToList(),
                Difficulty = task.Difficult,
                Time = task.Time,
                Lessons = task.LessonRecords.Select(l => new Lesson
                {
                    Code = l.Code,
                    Title = l.Title,
                    Content = l.Content,
                    Author = l.Author,
                    Supplements = l.SupplementKeys.ToList(),
                }).ToList(),
                TaskStatuses = task.TaskStatusRecords.Select(t => new TaskStatus
                    {
                        Id = t.Id,
                        Status = t.Status,
                        AutomationSystemStatus = t.AutomationSystemStatus,
                        StartedAt = t.StartedAt,
                        FinishedAt = t.FinishedAt,
                        Mark = t.Mark,
                        Fotos = t.Fotos.Select(f => new Foto
                        {
                            Key = f.FotoKey,
                            Comment = f.Comment,
                        }).ToList(),
                        UserKey = t.UserRecord?.Key.ToString() ?? string.Empty,
                        TaskCode = t.TaskRecord!.Code,
                        Recommendations = t.RecommendationRecords.Select(r => new Recommendation
                        {
                            Text = r.Text,
                            FileKeys = r.FileKeys,
                        }).ToList(),
                        Defects = t.DefectRecords.Select(d => new Defect
                        {
                            FileKey = d.FileKey,
                            Codes = d.Codes,
                            Comment = d.Comment,
                            X1 = d.X1,
                            Y1 = d.Y1,
                            X2 = d.X2,
                            Y2 = d.Y2,
                        }).ToList(),
                    }).ToList()
            };
    }
    
    public async Task<List<Task>> Get()
    {
        var taskRecords = await _dbContext.Tasks
            .Include(x => x.LessonRecords)
            .Include(x => x.TaskStatusRecords)
            .ThenInclude(taskStatusRecord => taskStatusRecord.UserRecord)
            .Include(taskRecord => taskRecord.TaskStatusRecords).ThenInclude(taskStatusRecord => taskStatusRecord.Fotos)
            .Include(taskRecord => taskRecord.TaskStatusRecords)
            .ThenInclude(taskStatusRecord => taskStatusRecord.RecommendationRecords)
            .Include(taskRecord => taskRecord.TaskStatusRecords)
            .ThenInclude(taskStatusRecord => taskStatusRecord.DefectRecords)
            .AsNoTracking()
            .ToListAsync();

        var tasks = taskRecords
            .Select(x => new Task
            {
                Content = x.Content,
                Code = x.Code,
                Title = x.Title,
                Supplements = x.SupplementKeys.ToList(),
                Difficulty = x.Difficult,
                Time = x.Time,
                Lessons = x.LessonRecords.Select(l => new Lesson
                {
                    Code = l.Code,
                    Title = l.Title,
                    Content = l.Content,
                    Author = l.Author,
                    Supplements = l.SupplementKeys.ToList(),
                }).ToList(),
                TaskStatuses = x.TaskStatusRecords.Select(t => new TaskStatus
                    {
                        Id = t.Id,
                        Status = t.Status,
                        AutomationSystemStatus = t.AutomationSystemStatus,
                        StartedAt = t.StartedAt,
                        FinishedAt = t.FinishedAt,
                        Mark = t.Mark,
                        Fotos = t.Fotos.Select(f => new Foto
                        {
                            Key = f.FotoKey,
                            Comment = f.Comment,
                        }).ToList(),
                        UserKey = t.UserRecord?.Key.ToString() ?? string.Empty,
                        TaskCode = t.TaskRecord!.Code,
                        Recommendations = t.RecommendationRecords.Select(r => new Recommendation
                        {
                            Text = r.Text,
                            FileKeys = r.FileKeys,
                        }).ToList(),
                        Defects = t.DefectRecords.Select(d => new Defect
                        {
                            FileKey = d.FileKey,
                            Codes = d.Codes,
                            Comment = d.Comment,
                            X1 = d.X1,
                            Y1 = d.Y1,
                            X2 = d.X2,
                            Y2 = d.Y2,
                        }).ToList(),
                    }).ToList()
            }).ToList();
        
        return tasks;
    }

    public async Task<int> Create(Task task)
    {
        var taskRecord = new TaskRecord
        {
            Code = task.Code,
            Title = task.Title,
            Content = task.Content,
            Difficult = task.Difficulty,
            SupplementKeys = task.Supplements.ToArray(),
            Time = task.Time,
        };

        await _dbContext.Tasks.AddAsync(taskRecord);
        return await _dbContext.SaveChangesAsync();
    }

    public async Task<int> Update(Task task)
    {
        return await _dbContext.Tasks
            .Where(x => x.Code.Equals(task.Code))
            .ExecuteUpdateAsync(x => x
                .SetProperty(b => b.Title, b => task.Title)
                .SetProperty(b => b.Content, b => task.Content)
                .SetProperty(b => b.Difficult, b => task.Difficulty)
                .SetProperty(b => b.Time, b => task.Time)
                .SetProperty(b => b.SupplementKeys, b => task.Supplements.ToArray()));
    }

    public async Task<int> Delete(string code)
    {
        return await _dbContext.Tasks
            .Where(x => x.Code.Equals(code))
            .ExecuteDeleteAsync();
    }
}