namespace backend.DataAccess.Entities;

public class TaskStatusRecord
{
    public int Id { get; set; }
    
    public string Status { get; set; }
    
    public string AutomationSystemStatus { get; set; }
    
    public DateTime StartedAt { get; set; }
    
    public DateTime FinishedAt { get; set; }
    
    public int? Mark { get; set; }
    
    public string[] FotoKeys { get; set; }
    
    public int UserRecordId { get; set; }
    
    public UserRecord? UserRecord { get; set; }
    
    public int TaskRecordId { get; set; }
    
    public TaskRecord? TaskRecord { get; set; }
    
    public List<RecommendationRecord> RecommendationRecords { get; set; }
}