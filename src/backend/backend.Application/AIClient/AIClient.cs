using System.Net.Http.Headers;
using backend.Core.JsonModels;
using backend.Core.Options;
using Microsoft.Extensions.Options;
using Newtonsoft.Json;

namespace backend.Application.Services;

public class AIClient : IAIClient
{
    private readonly AiOptions _aiOptions;

    public AIClient(IOptions<AiOptions> aiOptions)
    {
        _aiOptions = aiOptions.Value;
    }

    public async Task<List<JsonDefect>> CheckImage(
        Stream stream,
        string fileName,
        CancellationToken cancellationToken)
    {
        using var client = new HttpClient { BaseAddress = new Uri($"{_aiOptions.AS_2024_ENV_HOST}:{_aiOptions.AS_2024_ENV_PORT}") };

        using var form = new MultipartFormDataContent();
        var fileContent = new StreamContent(stream);
        fileContent.Headers.ContentType = MediaTypeHeaderValue.Parse("multipart/form-data");
        form.Add(fileContent, "file", fileName);

        //response
        var response = await client.PostAsync("/check", form, cancellationToken);
        response.EnsureSuccessStatusCode();
        var result = await response.Content.ReadAsStringAsync(cancellationToken);

        // parsing
        return JsonConvert.DeserializeObject<List<JsonDefect>>(result);
    }
}
