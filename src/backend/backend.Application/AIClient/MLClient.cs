using backend.Application.Services;
using backend.Core.JsonModels;

namespace backend.Application.AIClient;

public class MLClient : IAIClient
{
    public Task<List<JsonDefect>> CheckImage(Stream stream, string fileName, CancellationToken cancellationToken)
    {
        throw new NotImplementedException();
    }
}