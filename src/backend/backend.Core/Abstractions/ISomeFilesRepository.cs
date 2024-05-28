using backend.Core.Models;

namespace backend.Core.Abstractions;

public interface ISomeFilesRepository
{
    Task<List<SomeFile>> Get();
    Task<int> Create(SomeFile someFile);
    Task<int> Update(SomeFile someFile);
    Task<int> Delete(Guid key);
}