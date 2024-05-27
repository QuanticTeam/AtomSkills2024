using backend.Core.Abstractions;
using backend.Core.Models;

namespace backend.Application.Services;

public class SomethingsService : ISomethingsService
{
    private readonly ISomethingsRepository _repository;
    
    public SomethingsService(ISomethingsRepository repository)
    {
        _repository = repository;
    }

    public async Task<List<Something>> GetAll()
    {
        return await _repository.Get();
    }

    public async Task<Something?> Get(Guid key)
    {
        var orders = await _repository.Get();
        return orders.FirstOrDefault(x => x.Key.Equals(key));
    }

    public async Task<int> Create(Something something)
    {
        return await _repository.Create(something);
    }

    public async Task<int> Update(Something something)
    {
        return await _repository.Update(something);
    }

    public async Task<int> Delete(Guid key)
    {
        return await _repository.Delete(key);
    }
}