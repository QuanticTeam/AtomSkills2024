using backend.Core.Abstractions;
using backend.Core.Models;

namespace backend.Application.Services;

public class SomeFilesService : ISomeFilesService
{
    private readonly ISomeFilesRepository _repository;
    
    public SomeFilesService(ISomeFilesRepository repository)
    {
        _repository = repository;
    }

    public async Task<List<SomeFile>> GetAll()
    {
        return await _repository.Get();
    }

    public async Task<SomeFile?> Get(Guid key)
    {
        var orders = await _repository.Get();
        return orders.FirstOrDefault(x => x.Key.Equals(key));
    }

    public async Task<int> Create(SomeFile someFile)
    {
        return await _repository.Create(someFile);
    }

    public async Task<int> Update(SomeFile someFile)
    {
        return await _repository.Update(someFile);
    }

    public async Task<int> Delete(Guid key)
    {
        return await _repository.Delete(key);
    }
}