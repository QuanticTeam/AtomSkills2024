namespace backend.Core.Models;

public class User
{
    public User(Guid key, string login, string password, string role)
    {
        Key = key;
        Login = login;
        Password = password;
        Role = role;
    }
    
    public Guid Key { get; set; }
    
    public string Login { get; set; }
    
    public string Password { get; set; }
    
    public string Role { get; set; }
}