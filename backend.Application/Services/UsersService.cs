using backend.Core.Abstractions;
using backend.Core.models;
using backend.DataAccess.Repositories;

namespace backend.Application.Services;

public class UsersService : IUsersService
{
    private readonly IUsersRepository _repository;
    
    public UsersService(IUsersRepository repository)
    {
        _repository = repository;
    }

    public async Task<List<User>> GetAll()
    {
        return await _repository.Get();
    }

    public async Task<User?> Get(Guid key)
    {
        var orders = await _repository.Get();
        return orders.FirstOrDefault(x => x.Key.Equals(key));
    }

    public async Task<int> Create(User user)
    {
        return await _repository.Create(user);
    }

    public async Task<int> Update(User user)
    {
        return await _repository.Update(user);
    }

    public async Task<int> Delete(Guid key)
    {
        return await _repository.Delete(key);
    }
}