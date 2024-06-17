namespace backend.Contracts;

public record UploadFileResponse
{
    public string FileKey { get; set; }
    
    public string FileName { get; set; }
}