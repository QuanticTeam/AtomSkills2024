namespace backend.Contracts.Rank;

public class RankTasksResponse
{
    public List<RankTask> RankTasks { get; set; }
}

public class RankTask
{
    public string Code { get; set; }
    
    public string Title { get; set; }
    
    public int CountTwo { get; set; }
    
    public int CountThree { get; set; }
    
    public int CountFour { get; set; }
    
    public int CountFive { get; set; }
}