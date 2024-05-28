namespace backend.Contracts;

public record EditSomethingRequest
{
    public Guid Key { get; set; }
    
    public string Name { get; set; }
    
    public double Number { get; set; }
    
    public int Integer { get; set; }
    
    public DateTime DateTime { get; set; }
    
    public IFormFile File { get; set; }
}