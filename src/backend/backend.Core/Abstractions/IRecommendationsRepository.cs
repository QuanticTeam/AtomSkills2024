using backend.Core.Models;

namespace backend.Core.Abstractions;

public interface IRecommendationsRepository
{
    Task<List<Recommendation>> Get();
    Task<int> Create(Recommendation recommendation);
}