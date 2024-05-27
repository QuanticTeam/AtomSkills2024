namespace backend.Contracts;

public class SingUpRequest
{
    public string Login { get; set; }
    
    public string Password { get; set; }
    
    public string Role { get; set; }
}