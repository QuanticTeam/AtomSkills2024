namespace backend.Contracts.OM;

public class VerifiedTaskRequest
{
    public int TaskId { get; set; } 
    
    public int Mark { get; set; }
    
    public List<DefectRequest> Defects { get; set; }
    
}

public class DefectRequest
{
    public string FileKey { get; set; }
    
    public List<string> Codes { get; set; }
    
    public string Comment { get; set; }
    
    public int? X1 { get; set; }
    
    public int? Y1 { get; set; }
    
    public int? X2 { get; set; }

    public int? Y2 { get; set; }
}