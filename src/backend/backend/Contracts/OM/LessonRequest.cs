using backend.Core.Models;

namespace backend.Contracts.OM;

public record LessonRequest
{
    public required string Code { get; init; }
}