namespace backend.Core.Models;

public class Topic
{
    public string Code { get; set; }
    
    public string Title { get; set; }
    
    public string Description { get; set; }
    
    public List<Lesson> Lessons { get; set; }
    
    public List<Trait> Traits { get; set; }
}