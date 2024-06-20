namespace backend.Extensions;

public static class StringExtension
{
    public static string ToFirstLetterUpper(this string input)
    {
        return char.ToUpper(input[0]) + input[1..];
    }

    public static string GetMinIoFileName(this string originalFileName)
    {
        var fileExtension = originalFileName.Split('.').Last();
        return $"{Guid.NewGuid().ToString()}.{fileExtension}";
    }
    
    public static string Unescape(this string input)
    {
        return Uri.UnescapeDataString(input);
    }

    public static string Escape(this string input)
    {
        return Uri.EscapeDataString(input);
    }
}