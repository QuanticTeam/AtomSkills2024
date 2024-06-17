namespace backend.DataAccess.Entities;

public record TopicRecord
{
    public int Id { get; set; }
    
    public string Code { get; set; }
    
    public string Title { get; set; }
    
    public string Description { get; set; }
    
    public List<TraitRecord> TraitRecords { get; set; }
    
    public List<LessonRecord> LessonRecords { get; set; }
}