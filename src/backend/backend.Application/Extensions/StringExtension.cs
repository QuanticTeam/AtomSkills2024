namespace backend.Application.Extensions;

public static class StringExtension
{
    public static string Unescape(this string input)
    {
        return Uri.UnescapeDataString(input);
    }

    public static string Escape(this string input)
    {
        return Uri.EscapeDataString(input);
    }

    public static string GetMinIoFileName(this string originalFileName)
    {
        var fileExtension = originalFileName.Split('.').Last();
        return $"{Guid.NewGuid().ToString()}.{fileExtension}";
    }
}