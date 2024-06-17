namespace backend.DataAccess.Entities;

public record TaskRecord
{
    public int Id { get; set; }
    
    public string Code { get; set; }
    
    public string Title { get; set; }
    
    public string Content { get; set; }
    
    public double Difficult { get; set; }
    
    public int Time { get; set; }
    
    public string[] SupplementKeys { get; set; }
    
    public List<LessonRecord> LessonRecords { get; set; }
    
    public List<TaskStatusRecord> TaskStatusRecords { get; set; }
}