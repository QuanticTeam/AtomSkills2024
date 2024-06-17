namespace backend.Core.Models;

public class Task
{
    public string Code { get; set; }
    
    public string Title { get; set; }
    
    public string Content { get; set; }
    
    public double Difficulty { get; set; }
    
    public int Time { get; set; }
    
    public List<string> Supplements { get; set; }
    
    public List<Lesson> Lessons { get; set; }
    
    public List<TaskStatus> TaskStatuses { get; set; }
}