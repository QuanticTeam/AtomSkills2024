using backend.Core.models;

namespace backend.DataAccess.Repositories;

public interface IUsersRepository
{
    Task<List<User>> Get();
    Task<int> Create(User user);
    Task<int> Update(User user);
    Task<int> Delete(Guid key);
}