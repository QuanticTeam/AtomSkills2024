namespace backend.Contracts;

public record UploadFileRequest
{
    
    public IFormFile File { get; set; }
}