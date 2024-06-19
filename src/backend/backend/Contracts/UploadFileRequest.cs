namespace backend.Contracts;

public record UploadFileRequest
{
    public IFormFile File { get; set; }
    public string? Description { get; set; }
}