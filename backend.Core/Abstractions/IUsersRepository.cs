using backend.Core.Models;

namespace backend.Core.Abstractions;

public interface IUsersRepository
{
    Task<List<User>> Get();
    Task<int> Create(User user);
    Task<int> Update(User user);
    Task<int> Delete(Guid key);
}