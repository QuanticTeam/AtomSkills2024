namespace backend.Core.Models;

public class Something
{
    public Guid Key { get; set; }
    
    public string Name { get; set; }
    
    public double Number { get; set; }
    
    public int Integer { get; set; }
    
    public DateTime DateTime { get; set; }
    
    public string[] FileKeys { get; set; }
}