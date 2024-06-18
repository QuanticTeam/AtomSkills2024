namespace backend.Contracts.OM;
using TaskStatus = Core.Models.TaskStatus;

public record TaskStatusesResponse
{
    public required List<TaskStatus> Items { get; init; }
}