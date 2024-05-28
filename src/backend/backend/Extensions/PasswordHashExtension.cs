namespace backend.Extensions;

public static class PasswordHashExtension
{
    public static string Generate(this string password) =>
        BCrypt.Net.BCrypt.EnhancedHashPassword(password);

    public static bool Verify(this string hashedPassword, string password) =>
        BCrypt.Net.BCrypt.EnhancedVerify(password, hashedPassword);
}