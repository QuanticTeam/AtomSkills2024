namespace backend.Core.Models;

public class TaskStatus
{
    public int Id { get; set; }
    
    public string Status { get; set; }
    
    public string AutomationSystemStatus { get; set; }
    
    public DateTime StartedAt { get; set; }
    
    public DateTime? FinishedAt { get; set; }
    
    public int? Mark { get; set; }
    
    public List<Foto> Fotos { get; set; }
    
    public string UserKey { get; set; }
    
    public string TaskCode { get; set; }
    
    public List<Recommendation> Recommendations { get; set; }
    
    public List<Defect> Defects { get; set; }
}