using backend.Core.Models;

namespace backend.Contracts;

public class SignUpRequest
{
    public string Login { get; set; }
    
    public string Password { get; set; }
    
    public UserRole Role { get; set; }
    
    public string? FirstName { get; set; }
    
    public string? MiddleName { get; set; }
    
    public string? LastName { get; set; }
    
    public string? Email { get; set; }
    
    public string? Phone { get; set; }
}
