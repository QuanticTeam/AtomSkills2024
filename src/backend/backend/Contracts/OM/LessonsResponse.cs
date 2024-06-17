using backend.Core.Models;

namespace backend.Contracts.OM;

public record LessonsResponse
{
    public required List<Trait> Tags { get; init; } = new List<Trait>();
}