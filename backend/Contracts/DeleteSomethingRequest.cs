namespace backend.Contracts;

public record DeleteSomethingRequest
{
    public Guid Key { get; set; }
}