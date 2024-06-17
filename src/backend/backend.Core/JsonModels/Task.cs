namespace backend.Core.JsonModels;

public class JsonTask
{
    public string Code { get; set; }
    public string Title { get; set; }
    public string Content { get; set; }
    public List<JsonSupplement> Supplement { get; set; }
    public double Difficulty { get; set; }
    public int Time { get; set; }
}