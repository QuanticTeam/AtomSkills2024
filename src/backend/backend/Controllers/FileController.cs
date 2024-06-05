using System.Net.Mime;
using backend.Contracts;
using backend.Core.Abstractions;
using backend.Core.Models;
using backend.Notifications;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.SignalR;

namespace backend.Controllers;

[ApiController]
[Route("[controller]")]
public class FileController : ControllerBase
{
    private readonly ISomeFilesService _someFilesService;
    private readonly IHubContext<ToastNotificationHub> _hubContext;
    private readonly IMinIoFileService _minIoFileService;

    public FileController(
        ISomeFilesService someFilesService,
        IHubContext<ToastNotificationHub> hubContext,
        IMinIoFileService minIoFileService)
    {
        _someFilesService = someFilesService;
        _hubContext = hubContext;
        _minIoFileService = minIoFileService;
    }

    [AllowAnonymous]
    [HttpPost("Test")]
    public async Task<ActionResult<string>> Test()
    {
        await _hubContext.Clients.All.SendAsync("ToastNotification", "success test");
        return Ok("Test");
    }
    
    [Authorize]
    [HttpGet("Download/{id:guid}")]
    public async Task<FileStreamResult> Download(Guid id)
    {
        var file = await _someFilesService.Get(id);
        var stream = new MemoryStream(file!.Content);
        return File(stream, MediaTypeNames.Application.Octet, file.Name);
    }
    
    [Authorize]
    [HttpGet("DownloadFromMinIo/{fileName}")]
    public async Task<FileStreamResult> DownloadFromMinIo(string fileName)
    {
        Stream stream = await _minIoFileService.Download(fileName);
        return File(stream, MediaTypeNames.Application.Octet, fileName);
    }
    
    [Authorize]
    [HttpPost("Upload")]
    public async Task<ActionResult<UploadFileResponse>> Upload(IFormFile request)
    {
        var length = request.Length;
        if (length < 0)
            return BadRequest("File not founded, try again");

        await using var fileStream = request.OpenReadStream();
        var bytes = new byte[length];
        _ = await fileStream.ReadAsync(bytes.AsMemory(0, (int)length));

        var file = new SomeFile
        {
            Key = Guid.NewGuid(),
            Name = request.FileName,
            ContentType = request.ContentType,
            Content = bytes,
        };

        var successFile = await _someFilesService.Create(file);
        
        if (successFile == 0)
            return BadRequest("Something error, file does not upload");
            
        return Ok(new UploadFileResponse{ FileKey = file.Key.ToString(), FileName = file.Name });
    }
    
    [Authorize]
    [HttpPost("UploadWithMinIo")]
    public async Task<ActionResult<UploadFileResponse>> UploadWithMinIo(IFormFile request)
    {
        var metaData = new Dictionary<string, string>()
        {
            {
                "x-amz-meta-owner", 
                HttpContext.User.Claims.FirstOrDefault(x => x.Type.Equals("userId"))?.Value ?? string.Empty
            },
            {
                "x-amz-meta-original-file-name", 
                request.FileName
            },
        }; 
        
        var file = new MinIoFileModel(Guid.NewGuid().ToString(), request.ContentType, request.OpenReadStream(), metaData);
        
        var successFile = await _minIoFileService.Upload(file);

        if (successFile == 0)
            return BadRequest("Something error, try again");
        
        return Ok(new UploadFileResponse{ FileKey = file.FileName, FileName = file.MetaData["x-amz-meta-original-file-name"]});
    }
    
}