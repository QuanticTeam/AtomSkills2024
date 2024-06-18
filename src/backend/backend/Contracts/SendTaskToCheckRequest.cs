namespace backend.Contracts;

public class SendTaskToCheckRequest
{
    public int TaskStatusId { get; set; }
    
    public List<FileKeyAndDescription> FileKeyAndDescriptions { get; set; }
}

public class FileKeyAndDescription
{
    public string FileKey { get; set; }
    
    public string Description { get; set; }
}