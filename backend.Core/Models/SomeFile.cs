namespace backend.Core.Models;

public class SomeFile
{
    public SomeFile(Guid key, string name, string contentType, byte[] content)
    {
        Key = key;
        Name = name;
        ContentType = contentType;
        Content = content;
    }
    
    public Guid Key { get; set; }
    
    public string Name { get; set; }
    
    public string ContentType { get; set; }
    
    public byte[] Content { get; set; }
}