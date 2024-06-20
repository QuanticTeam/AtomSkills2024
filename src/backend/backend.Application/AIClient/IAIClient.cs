using backend.Core.JsonModels;

namespace backend.Application.Services;

public interface IAIClient
{
    Task<List<JsonDefect>> CheckImage(
        Stream stream,
        string fileName,
        CancellationToken cancellationToken);
}