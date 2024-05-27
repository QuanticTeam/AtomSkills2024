using backend.Core.models;

namespace backend.Core.Abstractions;

public interface IUsersService
{
    Task<List<User>> GetAll();
    Task<User?> Get(Guid key);
    Task<int> Create(User user);
    Task<int> Update(User user);
    Task<int> Delete(Guid key);
}