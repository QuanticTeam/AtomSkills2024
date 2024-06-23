using System.Net.Http.Headers;
using backend.Application.Services;
using backend.Core.JsonModels;
using backend.Core.Options;
using Microsoft.Extensions.Options;
using Microsoft.IdentityModel.Protocols.Configuration;
using Microsoft.IdentityModel.Tokens;
using Newtonsoft.Json;

namespace backend.Application.AIClient;

public class MLClient : IAIClient
{
    private readonly MLOptions _mlOptions;

    public MLClient(IOptions<MLOptions> mlOptions)
    {
        _mlOptions = mlOptions.Value;

        if (_mlOptions.AS_2024_ENV_URI.IsNullOrEmpty())
        {
            throw new InvalidConfigurationException($"Don't know ML Options: {nameof(_mlOptions.AS_2024_ENV_URI)}.");
        }
    }

    public async Task<List<JsonDefect>> CheckImage(Stream stream, string fileName, CancellationToken cancellationToken)
    {
        try 
        {
            using var client = new HttpClient { BaseAddress = new Uri($"{_mlOptions.AS_2024_ENV_URI}") };

            using var form = new MultipartFormDataContent();
            var fileContent = new StreamContent(stream);
            fileContent.Headers.ContentType = MediaTypeHeaderValue.Parse("multipart/form-data");
            form.Add(fileContent, "file", fileName);

            //response
            var response = await client.PostAsync("/check", form, cancellationToken);
            response.EnsureSuccessStatusCode();
            var result = await response.Content.ReadAsStringAsync(cancellationToken);

            JsonConvert.DeserializeXmlNode(result);

            var res = JsonConvert.DeserializeAnonymousType(result, new JsonDefect());

            return Enumerable.Empty<JsonDefect>().ToList();
        }
        catch 
        {
            return Enumerable.Empty<JsonDefect>().ToList();
        }
    }
}