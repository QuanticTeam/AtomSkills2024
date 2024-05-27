namespace backend.DataAccess.Entities;

public record SomethingRecord
{
    public int Id { get; set; }
    
    public Guid Key { get; set; }
    
    public string Name { get; set; }
    
    public double Number { get; set; }
    
    public int Integer { get; set; }
    
    public DateTime DateTime { get; set; }
    
    public Guid FileKey { get; set; }
}