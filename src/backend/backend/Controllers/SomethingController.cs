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
public class SomethingController : ControllerBase
{
    private readonly ISomethingsService _somethingsService;
    private readonly ISomeFilesService _someFilesService;
    private readonly IHubContext<ToastNotificationHub> _hubContext;
    private readonly IMinIoFileService _minIoFileService;

    public SomethingController(
        ISomethingsService somethingsService,
        ISomeFilesService someFilesService,
        IHubContext<ToastNotificationHub> hubContext,
        IMinIoFileService minIoFileService)
    {
        _somethingsService = somethingsService;
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
    
    [HttpGet("Get/{id:guid}")]
    public async Task<ActionResult<Something>> Get(Guid id)
    {
        var result = await _somethingsService.Get(id);
        
        if (result == null) 
            return BadRequest("Something not founded");

        return result;
    }
    
    [HttpGet("GetAll")]
    public async Task<ActionResult<List<Something>>> GetAll()
    {
        return await _somethingsService.GetAll();
    }
    
    [HttpGet("Download/{id:guid}")]
    public async Task<FileStreamResult> Download(Guid id)
    {
        var file = await _someFilesService.Get(id);
        var stream = new MemoryStream(file!.Content);
        return File(stream, file.ContentType, file.Name);
    }
    
    [HttpGet("DownloadFromMinIo/{fileName}")]
    public async Task<FileStreamResult> DownloadFromMinIo(string fileName)
    {
        (Stream stream, var contentType) = await _minIoFileService.Download(fileName);
        return File(stream, contentType, fileName);
    }

    [HttpPost("Create")]
    public async Task<ActionResult<int>> Create([FromForm] CreateSomethingRequest request)
    {
        var length = request.File.Length;
        if (length < 0)
            return BadRequest();

        await using var fileStream = request.File.OpenReadStream();
        var bytes = new byte[length];
        _ = await fileStream.ReadAsync(bytes.AsMemory(0, (int)length));

        var file = new SomeFile(Guid.NewGuid(), request.File.FileName, request.File.ContentType, bytes);
        
        var something = new Something(Guid.NewGuid(), request.Name, request.Number, request.Integer, request.DateTime, file.Key);

        var successSomething = await _somethingsService.Create(something);
        var successFile = await _someFilesService.Create(file);
        
        return Ok(successFile + successSomething);
    }
    
    [HttpPost("CreateWithMinIo")]
    public async Task<ActionResult<int>> CreateWithMinIo([FromForm] CreateSomethingRequest request)
    {
        var file = new MinIoFileModel(request.File.FileName, request.File.ContentType, request.File.OpenReadStream());
        
        var something = new Something(Guid.NewGuid(), request.Name, request.Number, request.Integer, request.DateTime, Guid.NewGuid());

        var successSomething = await _somethingsService.Create(something);
        var successFile = await _minIoFileService.Upload(file);
        
        return Ok(successFile + successSomething);
    }

    [HttpPost("Edit")]
    public async Task<ActionResult<int>> Edit([FromForm] EditSomethingRequest request)
    {
        var something = await _somethingsService.Get(request.Key);

        if (something == null)
        {
            return BadRequest("No something");
        }
        
        var length = request.File.Length;
        if (length < 0)
            return BadRequest();

        await using var fileStream = request.File.OpenReadStream();
        var bytes = new byte[length];
        _ = await fileStream.ReadAsync(bytes.AsMemory(0, (int)length));

        var file = new SomeFile(Guid.NewGuid(), request.File.FileName, request.File.ContentType, bytes);

        var successFile = await _someFilesService.Create(file);
        var successSomething = await _somethingsService.Update(new Something(something.Key, request.Name, request.Number, request.Integer, request.DateTime, file.Key));

        return Ok(successFile + successSomething);
    }
    
    [HttpPost("Delete")]
    public async Task<ActionResult<int>> Delete([FromBody] DeleteSomethingRequest request)
    {
        return Ok(await _somethingsService.Delete(request.Key));
    }
}