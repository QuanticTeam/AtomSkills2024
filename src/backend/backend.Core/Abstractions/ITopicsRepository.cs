using backend.Core.Models;

namespace backend.Core.Abstractions;

public interface ITopicsRepository
{
    Task<List<Topic>> Get();
    Task<int> Create(Topic topic);
    Task<int> Update(Topic topic);
    Task<int> Delete(string code);
}