namespace backend.Contracts.Rank;

public class RankUsersResponse
{
    public List<UserRank> UserRanks { get; set; }
}

public class UserRank
{
    public string Name { get; set; }
    
    public int Minutes { get; set; }
    
    public int? Mark { get; set; }
}