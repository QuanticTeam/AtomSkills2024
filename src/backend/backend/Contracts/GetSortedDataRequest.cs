namespace backend.Contracts;

public class GetSortAndFilterRequest
{
    public required string OrderBy { get; set; } = string.Empty;
    public required bool Descending { get; init; } = false;
    public required List<Filter> Filters { get; set; } = new List<Filter>{};
    public string? OMCode { get; set; }
}

public class Filter
{
    public required string ColumnName { get; set; }

    // всегда один элемент массива для простых фильтров 
    public required List<string> Values { get; set; } = new List<string> {};

    public required FilterType FilterType { get; set; }
}

public enum FilterType
{
    Contains = 0, // substring
    Equals = 1,
    GreaterThan = 2,
    LessThan = 3,
    OneOf = 4, // для фильтрации по меткам (теги, темы и т.п.)
}