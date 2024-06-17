namespace backend.DataAccess.Entities;

public class RecommendationRecord
{
    public int Id { get; set; }
    
    public string Text { get; set; }
    
    public string[] FileKeys { get; set; }
    
    public int TaskStatusRecordId { get; set; }
    
    public TaskStatusRecord? TaskStatusRecord { get; set; }
}