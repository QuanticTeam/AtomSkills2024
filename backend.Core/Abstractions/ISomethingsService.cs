using backend.Core.models;

namespace backend.Core.Abstractions;

public interface ISomethingsService
{
    Task<List<Something>> GetAll();
    Task<Something?> Get(Guid key);
    Task<int> Create(Something something);
    Task<int> Update(Something something);
    Task<int> Delete(Guid key);
}