namespace backend.Contracts;

public record CreateSomethingRequest
{
    public string Name { get; set; }
    
    public double Number { get; set; }
    
    public int Integer { get; set; }
    
    public DateTime DateTime { get; set; }
    
    public List<string> FileKeys { get; set; }
}