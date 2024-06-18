namespace backend.Core.Models;

public class Defect
{
    public int Id { get; set; }
    
    public string FileKey { get; set; }
    
    public string[] Codes { get; set; }
    
    public string Comment { get; set; }
    
    public int? X1 { get; set; }
    
    public int? Y1 { get; set; }
    
    public int? X2 { get; set; }

    public int? Y2 { get; set; }
    
    public int TaskStatusRecordId { get; set; }
}