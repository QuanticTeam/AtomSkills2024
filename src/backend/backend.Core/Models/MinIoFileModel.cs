namespace backend.Core.Models;

public class MinIoFileModel
{
    public MinIoFileModel(string fileName, string contentType, Stream stream)
    {
        FileName = fileName;
        ContentType = contentType;
        Stream = stream;
    }
    
    public string FileName { get; set; }
    
    public string ContentType { get; set; }
    
    public Stream Stream { get; set; }
}