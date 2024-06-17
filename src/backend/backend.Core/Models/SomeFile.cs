namespace backend.Core.Models;

public class SomeFile
{
    public Guid Key { get; set; }
    
    public string Name { get; set; }
    
    public string ContentType { get; set; }
    
    public byte[] Content { get; set; }
}