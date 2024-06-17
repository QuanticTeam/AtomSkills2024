namespace backend.Core.Options;

public class ContentLoadOptions
{
    public string FolderPath { get; set; } = string.Empty;

    public string TraitFileTemplate { get; set; } = string.Empty;
    public string TopicFileTemplate { get; set; } = string.Empty;
    public string LessonFileTemplate { get; set; } = string.Empty;
    public string TaskFileTemplate { get; set; } = string.Empty;

}