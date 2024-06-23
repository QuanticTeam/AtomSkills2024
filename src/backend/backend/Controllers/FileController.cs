using System.Net.Http.Headers;
using System.Net.Mime;
using backend.Contracts;
using backend.Core.Abstractions;
using backend.Core.Models;
using backend.Core.Options;
using backend.Extensions;
using backend.Notifications;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.SignalR;
using Microsoft.Extensions.Options;
using Newtonsoft.Json;

namespace backend.Controllers;

[ApiController]
[Route("[controller]")]
public class FileController : ControllerBase
{
    private readonly IHubContext<ToastNotificationHub> _hubContext;
    private readonly IMinIoFileService _minIoFileService;

    private const string OwnerKey = "x-amz-meta-owner";
    private const string OriginalFileNameKey = "x-amz-meta-original-file-name";

    public FileController(
        IHubContext<ToastNotificationHub> hubContext,
        IMinIoFileService minIoFileService)
    {
        _hubContext = hubContext;
        _minIoFileService = minIoFileService;
    }

    [AllowAnonymous]
    [HttpPost("Test")]
    public async Task<ActionResult<string>> Test()
    {
        await _hubContext.Clients.All.SendAsync("ToastNotification", "success test");
        return StatusCode(StatusCodes.Status200OK,"Test");
    }
    
    [Authorize]
    [HttpGet("Download/{fileName}")]
    public async Task<FileStreamResult> Download(string fileName)
    {
        (Stream stream, var originalFileName, _) = await _minIoFileService.Download(fileName);
        return File(stream, MediaTypeNames.Application.Octet, originalFileName);
    }
    
    [Authorize]
    [HttpPost("Upload")]
    public async Task<ActionResult<UploadFileResponse>> Upload(
        [FromForm] UploadFileRequest request,
        [FromServices] IOptions<MLOptions> mlOptions)
    {
            var mlOption = mlOptions.Value;
            using var client = new HttpClient { BaseAddress = new Uri($"{mlOption.AS_2024_ENV_URI}") };

            using var form = new MultipartFormDataContent();
            var fileContent = new StreamContent(request.File.OpenReadStream());
            fileContent.Headers.ContentType = MediaTypeHeaderValue.Parse("image/jpeg");
            form.Add(fileContent, "files", request.File.FileName.Escape());

            //response
            var response = await client.PostAsync("/detect_defect", fileContent, default);
            // response.EnsureSuccessStatusCode();
            var result = await response.Content.ReadAsStringAsync(default);

            var xxx = JsonConvert.DeserializeXmlNode(result);

            //var res = JsonConvert.DeserializeAnonymousType(result, new JsonDefect());

            //return Enumerable.Empty<JsonDefect>().ToList();




        var metaData = new Dictionary<string, string>
        {
            {
                OwnerKey, 
                HttpContext.User.Claims.FirstOrDefault(x => x.Type.Equals("userId"))?.Value ?? string.Empty
            },
            {
                OriginalFileNameKey, 
                request.File.FileName.Escape()
            },
        }; 
        
        var file = new MinIoFileModel(request.File.FileName.GetMinIoFileName(), request.File.ContentType, request.File.OpenReadStream(), metaData);
        
        var successFile = await _minIoFileService.Upload(file);

        if (successFile == 0)
            return StatusCode(StatusCodes.Status400BadRequest,"Something error, try again");
        
        return StatusCode(StatusCodes.Status200OK, new UploadFileResponse{ FileKey = file.FileName, FileName = file.MetaData[OriginalFileNameKey]});
    }
    
    [Authorize]
    [HttpPost("GetMinIoData")]
    public async Task<ActionResult<GetMinIoDataResponse>> GetMinIoData(FileKeysRequest request)
    {
        var response = request.FileKeys.Select(x => new MinIoData
        {
            Code = x,
            OriginalNme = _minIoFileService.GetOriginalFileNameWithUnescape(x).GetAwaiter().GetResult(),
            Title = _minIoFileService.GetTitleWithUnescape(x).GetAwaiter().GetResult(),
        }).ToList();
        
        return StatusCode(StatusCodes.Status200OK, response);
    }
}