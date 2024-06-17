namespace backend.Core.Models;

public class TaskStatus
{
    public int Id { get; set; }
    
    public string Status { get; set; }
    
    public string AutomationSystemStatus { get; set; }
    
    public DateTime StartedAt { get; set; }
    
    public DateTime FinishedAt { get; set; }
    
    public int? Mark { get; set; }
    
    public string[] FotoKeys { get; set; }
    
    public string UserKey { get; set; }
    
    public int TaskId { get; set; }
    
    public List<Recommendation> Recommendations { get; set; }
}