using backend.Contracts;
using backend.Core.Abstractions;
using backend.Core.Models;
using backend.Notifications;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.SignalR;

namespace backend.Controllers;

public class UserController : ControllerBase
{
    private readonly IUsersService _usersService;
    private readonly IHubContext<ToastNotificationHub> _hubContext;

    public UserController(
        IUsersService usersService,
        IHubContext<ToastNotificationHub> hubContext)
    {
        _usersService = usersService;
        _hubContext = hubContext;
    }

    [HttpPost("Test")]
    public async Task<ActionResult<string>> Test()
    {
        await _hubContext.Clients.All.SendAsync("ToastNotification", "success test");
        return Ok("test");
    }
    
    [HttpPost("Login")]
    public async Task<ActionResult<string>> Login([FromBody] LoginRequest request)
    {
        var users = await _usersService.GetAll();
        var user = users.FirstOrDefault(x => x.Login.Equals(request.Login) && x.Password.Equals(request.Password));

        if (user == null)
        {
            return BadRequest("User is not founded");
        }

        return Ok(user.Role);
    }
    
    [HttpPost("SingUp")]
    public async Task<ActionResult<int>> SingUp([FromBody] SingUpRequest request)
    {
        var user = new User(Guid.NewGuid(), request.Login, request.Password, request.Role);
        return Ok(await _usersService.Create(user));
    }
}