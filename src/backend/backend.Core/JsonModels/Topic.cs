namespace backend.Core.JsonModels;

public class JsonTopic
{
    public string Code { get; set; }
    public string Title { get; set; }
    public List<string> Lessons { get; set; }
    public List<string> Traits { get; set; }
    public string Description { get; set; }
}