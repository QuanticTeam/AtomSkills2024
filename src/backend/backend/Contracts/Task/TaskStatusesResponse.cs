namespace backend.Contracts.OM;

public record TaskStatusesResponse
{
    public required List<ExtendedTaskStatus> Items { get; init; }
}