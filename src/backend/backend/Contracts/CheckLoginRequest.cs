namespace backend.Contracts;

public record CheckLoginRequest
{
    public string Login { get; set; }
}