namespace backend.Core.Models;

public class User
{
    public User(Guid key, string login, string password, string role, string? firstName, string? middleName, string? lastName, string? email, string? phone)
    {
        Key = key;
        Login = login;
        Password = password;
        Role = role;
        FirstName = firstName;
        MiddleName = middleName;
        LastName = lastName;
        Email = email;
        Phone = phone;
    }
    
    public Guid Key { get; set; }
    
    public string Login { get; set; }
    
    public string Password { get; set; }
    
    public string Role { get; set; }
    
    public string? FirstName { get; set; }
    
    public string? MiddleName { get; set; }
    
    public string? LastName { get; set; }
    
    public string? Email { get; set; }
    
    public string? Phone { get; set; }
}