using backend.Core.models;

namespace backend.DataAccess.Repositories;

public interface ISomethingsRepository
{
    Task<List<Something>> Get();
    Task<int> Create(Something something);
    Task<int> Update(Something something);
    Task<int> Delete(Guid key);
}