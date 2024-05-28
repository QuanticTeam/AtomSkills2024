namespace backend.FileUploads;

internal class MinIoFileModel
{
    internal MinIoFileModel(Guid guid, IFormFile file)
    {
        BucketId = guid;
        File = file;
    }
    
    internal Guid BucketId { get; set; }
    
    internal IFormFile File { get; set; }
}