namespace backend.Core.JsonModels;

public class JsonDefect
{
    public Area Area { get; set; }
    
    public string[] Features { get; set; }
}

public class Area
{
    public int X1 { get; set; }
    
    public int Y1 { get; set; }
    
    public int X2 { get; set; }
    
    public int Y2 { get; set; }
}