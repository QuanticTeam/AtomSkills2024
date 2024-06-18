namespace backend.DataAccess.Entities;

public record FotoRecord
{
    public int Id { get; set; }
    
    public string FotoKey { get; set; }
    
    public string Comment { get; set; }
    
    public int TaskStatusRecordId { get; set; }
    
    public TaskStatusRecord TaskStatusRecord { get; set; }
}