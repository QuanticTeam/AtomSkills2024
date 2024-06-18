namespace backend.Contracts.OM;

public record TaskStatusesRequest
{
    public required string Code { get; init; }
}