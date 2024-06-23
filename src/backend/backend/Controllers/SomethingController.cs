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

namespace backend.Controllers;

[ApiController]
[Route("[controller]")]
public class SomethingController : ControllerBase
{
    private readonly ISomethingsService _somethingsService;
    private readonly IMinIoFileService _minIoFileService;
    private readonly IHubContext<ToastNotificationHub> _hubContext;

    private readonly IContentLoadService _contentLoadService;
    private readonly IDownloadService _downloadService;
    
    public SomethingController(
        ISomethingsService somethingsService,
        IHubContext<ToastNotificationHub> hubContext,
        IMinIoFileService minIoFileService,
        IContentLoadService contentLoadService,
        IDownloadService downloadService)
    {
        _somethingsService = somethingsService;
        _hubContext = hubContext;
        _minIoFileService = minIoFileService;
        _contentLoadService = contentLoadService;
        _downloadService = downloadService;
    }

    [AllowAnonymous]
    [HttpPost("UploadContent")]
    public async Task<ActionResult<string>> UploadContent([FromServices] IContentLoadService contentLoadService)
    {
        var count = await _downloadService.Download();
        var dictionary = contentLoadService.Dictionary;
        return StatusCode(StatusCodes.Status200OK, $"Успешно загружено данных: Материалы по обучению {count}, словарь дефектов: {dictionary.Count}");
    }

    [Authorize]
    [HttpPost("Test")]
    public async Task<ActionResult<string>> Test(
        [FromServices] IContentLoadService contentLoadService,
        [FromServices] IOptions<MLOptions> mlOptions)
    {
        await _hubContext.Clients.All.SendAsync("ToastNotification", "success test");
        var userId = HttpContext.User.Claims.FirstOrDefault(x => x.Type.Equals("userId"))?.Value ?? string.Empty;
        if (HttpContext.User.IsInRole("Admin"))
        {
            var count = contentLoadService.Dictionary.Count;
            return StatusCode(StatusCodes.Status200OK, $"Успешно загружено данных: {count}");
        }
        return StatusCode(StatusCodes.Status200OK, "Not admin");
    }
    
    [Authorize]
    [HttpGet("Get/{id:guid}")]
    public async Task<ActionResult<GetSomethingResponse>> Get(Guid id)
    {
        var something = await _somethingsService.Get(id);

        if (something == null)
            return StatusCode(StatusCodes.Status404NotFound, "Something not founded");

        var somethingResponse = new GetSomethingResponse
        {
            Key = something.Key,
            Name = something.Name,
            Integer = something.Integer,
            Number = something.Number,
            DateTime = something.DateTime,
            Files = something.FileKeys.Select(x => new UploadFileResponse
            {
                FileKey = x,
                FileName = _minIoFileService.GetOriginalFileName(x).GetAwaiter().GetResult(),
            }).ToList()
        };

        return StatusCode(StatusCodes.Status200OK, somethingResponse);
    }
    
    [Authorize]
    [HttpGet("GetAll")]
    public async Task<ActionResult<List<GetSomethingResponse>>> GetAll()
    {
        var somethings = await _somethingsService.GetAll();

        var somethingResponses = somethings.Select(something => new GetSomethingResponse
        {
            Key = something.Key,
            Name = something.Name,
            Integer = something.Integer,
            Number = something.Number,
            DateTime = something.DateTime,
            Files = something.FileKeys.Select(x => new UploadFileResponse
            {
                FileKey = x,
                FileName = _minIoFileService.GetOriginalFileName(x).GetAwaiter().GetResult(),
            }).ToList()
        });
        
        return StatusCode(StatusCodes.Status200OK, somethingResponses);
    }
    
    [Authorize]
    [HttpPost("GetSortedData")]
    public async Task<ActionResult<List<GetSomethingResponse>>> GetSortedData(GetSortAndFilterRequest request)
    {
        var data = await _somethingsService.GetAll();

        var result = data.AsQueryable();

        // if (request.Filter != null)
        // {
        //     result = result.Where(FilterExtension.Filter<Something>(request.Filter));
        // }

        if (!string.IsNullOrEmpty(request.OrderBy))
        {
            result = result.Order<Something>(request.OrderBy!, request.Descending);
        }
        
        var somethingResponses = result.Select(something => new GetSomethingResponse
        {
            Key = something.Key,
            Name = something.Name,
            Integer = something.Integer,
            Number = something.Number,
            DateTime = something.DateTime,
            Files = something.FileKeys.Select(x => new UploadFileResponse
            {
                FileKey = x,
                FileName = _minIoFileService.GetOriginalFileName(x).GetAwaiter().GetResult(),
            }).ToList()
        });

        return StatusCode(StatusCodes.Status200OK, somethingResponses.ToList());
    }
    

    [Authorize]
    [HttpPost("Create")]
    public async Task<ActionResult<int>> Create([FromBody] CreateSomethingRequest request)
    {
        var something = new Something
        {
            Key = Guid.NewGuid(),
            Name = request.Name,
            Integer = request.Integer,
            Number = request.Number,
            DateTime = request.DateTime,
            FileKeys = request.FileKeys.ToArray(),
        };

        var successSomething = await _somethingsService.Create(something);
        
        return StatusCode(StatusCodes.Status200OK, successSomething);
    }

    [Authorize]
    [HttpPost("Edit")]
    public async Task<ActionResult<int>> Edit([FromBody] EditSomethingRequest request)
    {
        var something = await _somethingsService.Get(request.Key);

        if (something == null)
        {
            return StatusCode(StatusCodes.Status404NotFound, "No something");
        }

        var successSomething = await _somethingsService.Update(new Something
        {
            Key = something.Key,
            Name = request.Name,
            Number = request.Number,
            Integer = request.Integer,
            DateTime = request.DateTime,
            FileKeys = request.FileKeys.ToArray()
        });

        return StatusCode(StatusCodes.Status200OK, successSomething);
    }
    
    [Authorize]
    [HttpPost("Delete")]
    public async Task<ActionResult<int>> Delete([FromBody] DeleteSomethingRequest request)
    {
        return StatusCode(StatusCodes.Status200OK, await _somethingsService.Delete(request.Key));
    }
}