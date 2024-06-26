using backend.Core.Models;

namespace backend.Core.Abstractions;

public interface IDefectsRepository
{
    Task<List<Defect>> Get();
    Task<int> Create(Defect defect);
    Task<int> Update(int id, string fileKey);
}