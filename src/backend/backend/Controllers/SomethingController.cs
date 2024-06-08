using backend.Contracts;
using backend.Core.Abstractions;
using backend.Core.Models;
using backend.Extensions;
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
    private readonly IHubContext<ToastNotificationHub> _hubContext;
    public SomethingController(
        ISomethingsService somethingsService,
        IHubContext<ToastNotificationHub> hubContext)
    {
        _somethingsService = somethingsService;
        _hubContext = hubContext;
    }

    [Authorize]
    // [AllowAnonymous]
    [HttpPost("Test")]
    public async Task<ActionResult<string>> Test()
    {
        await _hubContext.Clients.All.SendAsync("ToastNotification", "success test");
        var userId = HttpContext.User.Claims.FirstOrDefault(x => x.Type.Equals("userId"))?.Value ?? string.Empty;
        if (HttpContext.User.IsInRole("Admin"))
        {
            return Ok("Admin");
        }
        return Ok("Not admin");
    }
    
    [Authorize]
    [HttpGet("Get/{id:guid}")]
    public async Task<ActionResult<Something>> Get(Guid id)
    {
        var result = await _somethingsService.Get(id);
        
        if (result == null) 
            return BadRequest("Something not founded");

        return result;
    }
    
    [Authorize]
    [HttpGet("GetAll")]
    public async Task<ActionResult<List<Something>>> GetAll()
    {
        return await _somethingsService.GetAll();
    }
    
    [Authorize]
    [HttpPost("GetSortedData")]
    public async Task<ActionResult<List<Something>>> GetSortedData(GetSortedDataRequest request)
    {
        var data = await _somethingsService.GetAll();

        var result = data.AsQueryable();

        if (request.Filter != null)
        {
            result = result.Where(FilterExtension.Filter<Something>(request.Filter));
        }

        if (!string.IsNullOrEmpty(request.OrderBy))
        {
            result = result.Order<Something>(request.OrderBy!, request.Descending);
        }

        return result.ToList();
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
        
        return Ok(successSomething);
    }

    [Authorize]
    [HttpPost("Edit")]
    public async Task<ActionResult<int>> Edit([FromBody] EditSomethingRequest request)
    {
        var something = await _somethingsService.Get(request.Key);

        if (something == null)
        {
            return BadRequest("No something");
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

        return Ok(successSomething);
    }
    
    [Authorize]
    [HttpPost("Delete")]
    public async Task<ActionResult<int>> Delete([FromBody] DeleteSomethingRequest request)
    {
        return Ok(await _somethingsService.Delete(request.Key));
    }
}