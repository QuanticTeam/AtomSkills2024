namespace backend.DataAccess.Entities;

public record LessonRecord
{
    public int Id { get; set; }
    
    public string Code { get; set; }
    
    public string Title { get; set; }
    
    public string Content { get; set; }
    
    public string Author { get; set; }
    
    public string[] SupplementKeys { get; set; }
    
    public List<TraitRecord> TraitRecords { get; set; }
    
    public List<TaskRecord> TaskRecords { get; set; }
    
    public List<TopicRecord> TopicRecords { get; set; }
}