using backend.Core.Models;

namespace backend.Core.Abstractions;

public interface ISomeFilesService
{
    Task<List<SomeFile>> GetAll();
    Task<SomeFile?> Get(Guid key);
    Task<int> Create(SomeFile someFile);
    Task<int> Update(SomeFile someFile);
    Task<int> Delete(Guid key);
}