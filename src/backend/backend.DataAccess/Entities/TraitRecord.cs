namespace backend.DataAccess.Entities;

public record TraitRecord
{
    public int Id { get; set; }
    
    public string Code { get; set; }
    
    public string Name { get; set; }
    
    public string Description { get; set; }
    
    public List<TopicRecord> TopicRecords { get; set; }
    
    public List<LessonRecord> LessonRecords { get; set; }
}