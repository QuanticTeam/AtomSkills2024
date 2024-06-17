namespace backend.Core.Models;

public class MinIoFileModel
{
    public MinIoFileModel(string fileName, string contentType, Stream stream, Dictionary<string, string> metaData)
    {
        FileName = fileName;
        ContentType = contentType;
        Stream = stream;
        MetaData = metaData;
    }
    
    public string FileName { get; set; }
    
    public string ContentType { get; set; }
    
    public Stream Stream { get; set; }
    
    public Dictionary<string, string> MetaData { get; set; }
}