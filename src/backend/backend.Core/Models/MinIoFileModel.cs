namespace backend.Core.Models;

public class MinIoFileModel
{
    public MinIoFileModel(Guid guid, string fileName, string contentType, Stream stream)
    {
        BucketId = guid;
        FileName = fileName;
        ContentType = contentType;
        Stream = stream;
    }
    
    public Guid BucketId { get; set; }
    
    public string FileName { get; set; }
    
    public string ContentType { get; set; }
    
    public Stream Stream { get; set; }
}