namespace backend.Core.JsonModels;

public class JsonLesson
{
    public string Code { get; set; }
    public string Title { get; set; }
    public string Content { get; set; }
    public List<string> Traits { get; set; }
    public List<JsonSupplement> Supplement { get; set; }
    public List<string> Tasks { get; set; }
    public string Author { get; set; }
}