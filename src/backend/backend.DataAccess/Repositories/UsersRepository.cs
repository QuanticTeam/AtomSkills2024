using backend.Core.Abstractions;
using backend.Core.Models;
using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;
using TaskStatus = backend.Core.Models.TaskStatus;

namespace backend.DataAccess.Repositories;

public class UsersRepository : IUsersRepository
{
    private readonly BackendDbContext _dbContext;
    
    public UsersRepository(BackendDbContext dbContext)
    {
        _dbContext = dbContext;
    }

    public async Task<List<User>> Get()
    {
        var userRecords = await _dbContext.Users
            .Include(x => x.TaskStatusRecords)
            .ThenInclude(taskStatusRecord => taskStatusRecord.TaskRecord)
            .AsNoTracking()
            .ToListAsync();

        var users = userRecords
            .Select(x => new User(
                x.Key, 
                x.Login, 
                x.Password, 
                x.Role, 
                x.FirstName, 
                x.MiddleName, 
                x.LastName, 
                x.Email, 
                x.Phone,
                x.TaskStatusRecords.Select(t => new TaskStatus
                {
                    Id = t.Id,
                    Status = t.Status,
                    AutomationSystemStatus = t.AutomationSystemStatus,
                    StartedAt = t.StartedAt,
                    FinishedAt = t.FinishedAt,
                    Mark = t.Mark,
                    UserKey = x.Key.ToString(),
                    TaskCode = t.TaskRecord!.Code,
                }).ToList()))
            .ToList();
        
        return users;
    }

    public async Task<int> Create(User user)
    {
        var userRecord = new UserRecord
        {
            Key = user.Key,
            Login = user.Login,
            Password = user.Password,
            Role = user.Role,
            FirstName = user.FirstName,
            MiddleName = user.MiddleName,
            LastName = user.LastName,
            Email = user.Email,
            Phone = user.Phone
        };

        await _dbContext.Users.AddAsync(userRecord);
        return await _dbContext.SaveChangesAsync();
    }

    public async Task<int> Update(User user)
    {
        return await _dbContext.Users
            .Where(x => x.Key.Equals(user.Key))
            .ExecuteUpdateAsync(x => x
                .SetProperty(b => b.Login, b => user.Login)
                .SetProperty(b => b.Password, b => user.Password)
                .SetProperty(b => b.Role, b => user.Role)
                .SetProperty(b => b.FirstName, b => user.FirstName)
                .SetProperty(b => b.MiddleName, b => user.MiddleName)
                .SetProperty(b => b.LastName, b => user.LastName)
                .SetProperty(b => b.Email, b => user.Email)
                .SetProperty(b => b.Phone, b => user.Phone));
    }

    public async Task<int> Delete(Guid key)
    {
        return await _dbContext.Users
            .Where(x => x.Key.Equals(key))
            .ExecuteDeleteAsync();
    }
}