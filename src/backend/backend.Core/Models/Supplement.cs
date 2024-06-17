namespace backend.Core.Models;

public class Supplement
{
    public string Key { get; set; }
    public string Title { get; set; }
    public string FilePath { get; set; }
    public string MimeType { get; set; }
    public bool IsLoaded { get; set; }
}