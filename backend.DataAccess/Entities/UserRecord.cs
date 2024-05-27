namespace backend.DataAccess.Entities;

public record UserRecord
{
    public int Id { get; set; }
    
    public Guid Key { get; set; }
    
    public string Login { get; set; }
    
    public string Password { get; set; }
    
    public string Role { get; set; }
}