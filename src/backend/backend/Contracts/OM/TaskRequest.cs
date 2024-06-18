namespace backend.Contracts.OM;

public record TaskRequest
{
    public required string Code { get; init; }
}