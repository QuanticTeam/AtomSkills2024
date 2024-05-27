namespace backend.Core.models;

public class Something
{
    public Something(Guid key, string name, double number, int integer, DateTime dateTime, Guid fileKey)
    {
        Key = key;
        Name = name;
        Number = number;
        Integer = integer;
        DateTime = dateTime;
        FileKey = fileKey;
    }
    
    public Guid Key { get; set; }
    
    public string Name { get; set; }
    
    public double Number { get; set; }
    
    public int Integer { get; set; }
    
    public DateTime DateTime { get; set; }
    
    public Guid FileKey { get; set; }
}