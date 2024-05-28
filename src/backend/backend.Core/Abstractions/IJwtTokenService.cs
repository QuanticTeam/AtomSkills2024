using backend.Core.Models;

namespace backend.Core.Abstractions;

public interface IJwtTokenService
{
    string GenerateToken(User user);
}