namespace backend.Core.Models;

public class Trait
{
    public string Code { get; set; }
    
    public string Name { get; set; }
    
    public string Description { get; set; }
    
    public List<Topic> TopicRecords { get; set; }
    
    public List<Lesson> LessonRecords { get; set; }
}