using backend.Core.Models;

namespace backend.Core.Abstractions;

public interface ITraitRepository
{
    Task<List<Trait>> Get();
    Task<int> Create(Trait trait);
    Task<int> Update(Trait trait);
    Task<int> Delete(string code);
}