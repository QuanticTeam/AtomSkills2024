namespace backend.Extensions;

public static class StringExtension
{
    public static string ToFirstLetterUpper(this string input)
    {
        return char.ToUpper(input[0]) + input[1..];
    }
}