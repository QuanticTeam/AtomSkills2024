using backend.Core.Models;

namespace backend.Contracts.OM;

public record TopicsResponse
{
    public required List<Topic> Tags { get; init; } = new List<Topic>();
}