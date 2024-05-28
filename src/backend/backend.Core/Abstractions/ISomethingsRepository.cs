using backend.Core.Models;

namespace backend.Core.Abstractions;

public interface ISomethingsRepository
{
    Task<List<Something>> Get();
    Task<int> Create(Something something);
    Task<int> Update(Something something);
    Task<int> Delete(Guid key);
}