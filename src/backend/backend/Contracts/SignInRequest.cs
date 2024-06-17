namespace backend.Contracts;

public record SignInRequest
{
    public string Login { get; set; }
    
    public string Password { get; set; }
}