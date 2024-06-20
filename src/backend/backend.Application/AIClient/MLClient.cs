using backend.Application.Services;
using backend.Core.JsonModels;
using backend.Core.Options;
using Microsoft.Extensions.Options;
using Microsoft.IdentityModel.Protocols.Configuration;
using Microsoft.IdentityModel.Tokens;

namespace backend.Application.AIClient;

public class MLClient : IAIClient
{
    private readonly MLOptions _mLOptions;

    public MLClient(IOptions<MLOptions> mlOptions)
    {
        _mLOptions = mlOptions.Value;

        if (_mLOptions.AS_2024_ENV_URI.IsNullOrEmpty())
        {
            throw new InvalidConfigurationException($"Don't know ML Options: {nameof(_mLOptions.AS_2024_ENV_URI)}.");
        }
    }

    public Task<List<JsonDefect>> CheckImage(Stream stream, string fileName, CancellationToken cancellationToken)
    {
        throw new NotImplementedException();
    }
}