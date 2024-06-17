namespace backend.Core.Models;

public class Lesson
{
    public string Code { get; set; }
    
    public string Title { get; set; }
    
    public string Content { get; set; }
    
    public string Author { get; set; }
    
    public List<string> Supplements { get; set; }
    
    public List<Trait> Traits { get; set; }
    
    public List<Task> Tasks { get; set; }
    
    public List<Topic> Topics { get; set; }
}