using backend.Core.models;
using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;

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
            .AsNoTracking()
            .ToListAsync();

        var users = userRecords
            .Select(x => new User(x.Key, x.Login, x.Password, x.Role))
            .ToList();
        
        return users;
    }

    public async Task<int> Create(User user)
    {
        var userRecord = new UserRecord()
        {
            Key = user.Key,
            Login = user.Login,
            Password = user.Password,
            Role = user.Role,
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
                .SetProperty(b => b.Role, b => user.Role));
    }

    public async Task<int> Delete(Guid key)
    {
        return await _dbContext.Users
            .Where(x => x.Key.Equals(key))
            .ExecuteDeleteAsync();
    }
}