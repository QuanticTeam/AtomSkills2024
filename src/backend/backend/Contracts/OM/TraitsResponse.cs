using backend.Core.Models;

namespace backend.Contracts.OM;

public record TraitsResponse
{
    public required List<Trait> Tags { get; init; } = new List<Trait>();
}