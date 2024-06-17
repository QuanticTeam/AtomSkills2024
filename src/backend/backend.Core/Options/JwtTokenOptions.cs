namespace backend.Core.Options;

public class JwtTokenOptions
{
    public string SecretKey { get; set; }
    
    public int ExpiresHours { get; set; }
}