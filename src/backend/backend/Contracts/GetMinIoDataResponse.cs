namespace backend.Contracts;

public class GetMinIoDataResponse
{
    public List<MinIoData> MinIoDatas { get; set; }
}

public class MinIoData
{
    public string Code { get; set; }
    
    public string OriginalNme { get; set; }
    
    public string Title { get; set; }
}