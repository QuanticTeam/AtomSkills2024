namespace backend.Contracts;

public class GetSortedDataRequest
{
    public string? OrderBy { get; set; }
    public bool Descending { get; init; } = false;
    public List<Filter>? Filter { get; set; }
}

public class Filter
{
    public string ColumnName { get; set; }

    public string Value { get; set; }

    public FilterType FilterType { get; set; }
}

public enum FilterType
{
    Contains = 0,
    Equals = 1,
    StartsWith = 2,
    EndsWith = 3,
    GreaterThan = 4,
    LessThan = 5,
}