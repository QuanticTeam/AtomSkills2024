using Task = backend.Core.Models.Task;

namespace backend.Contracts.OM;

public record TasksResponse
{
    public required List<Task> Tasks { get; init; } = new List<Task>();
}