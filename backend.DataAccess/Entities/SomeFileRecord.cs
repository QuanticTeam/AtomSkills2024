namespace backend.DataAccess.Entities;

public class SomeFileRecord
{
    public int Id { get; set; }
    
    public Guid Key { get; set; }
    
    public string Name { get; set; }
    
    public string ContentType { get; set; }
    
    public byte[] Content { get; set; }
}