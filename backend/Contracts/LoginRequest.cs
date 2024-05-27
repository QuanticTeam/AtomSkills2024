namespace backend.Contracts;

public record LoginRequest
{
    public string Login { get; set; }
    
    public string Password { get; set; }
}