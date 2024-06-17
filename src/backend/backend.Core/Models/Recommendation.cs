namespace backend.Core.Models;

public class Recommendation
{
    public string Text { get; set; }
    
    public string[] FileKeys { get; set; }
    
    public int TaskStatusRecordId { get; set; }
}