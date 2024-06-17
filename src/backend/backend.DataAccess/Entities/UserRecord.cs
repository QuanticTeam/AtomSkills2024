namespace backend.DataAccess.Entities;

public record UserRecord
{
    public int Id { get; set; }
    
    public Guid Key { get; set; }
    
    public string Login { get; set; }
    
    public string Password { get; set; }
    
    public string Role { get; set; }
    
    public string? FirstName { get; set; }
    
    public string? MiddleName { get; set; }
    
    public string? LastName { get; set; }
    
    public string? Email { get; set; }
    
    public string? Phone { get; set; }
    
    public List<TaskStatusRecord> TaskStatusRecords { get; set; }
}