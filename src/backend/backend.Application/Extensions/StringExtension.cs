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
}