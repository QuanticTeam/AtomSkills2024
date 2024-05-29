using backend.Core.Models;

namespace backend.Contracts;

public class SingUpRequest
{
    public string Login { get; set; }
    
    public string Password { get; set; }
    
    public UserRole Role { get; set; }
}