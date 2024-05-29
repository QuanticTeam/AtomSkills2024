using backend.Contracts;
using backend.Core.Abstractions;
using backend.Core.Models;
using backend.Extensions;
using backend.Notifications;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.SignalR;

namespace backend.Controllers;

public class UserController : ControllerBase
{
    private readonly IUsersService _usersService;
    private readonly IJwtTokenService _jwtTokenService;
    private readonly IHubContext<ToastNotificationHub> _hubContext;

    public UserController(
        IUsersService usersService,
        IJwtTokenService jwtTokenService,
        IHubContext<ToastNotificationHub> hubContext)
    {
        _usersService = usersService;
        _jwtTokenService = jwtTokenService;
        _hubContext = hubContext;
    }

    [Authorize]
    [HttpPost("Test")]
    public async Task<ActionResult<string>> Test()
    {
        await _hubContext.Clients.All.SendAsync("ToastNotification", "success test");
        var userId = HttpContext.User.Claims.FirstOrDefault(x => x.Type.Equals("userId"))?.Value ?? string.Empty;
        return Ok(userId);
    }
    
    [HttpPost("Login")]
    public async Task<ActionResult<string>> Login([FromBody] LoginRequest request)
    {
        var users = await _usersService.GetAll();
        var user = users.FirstOrDefault(x => x.Login.Equals(request.Login) && x.Password.Verify(request.Password));

        if (user == null)
        {
            return BadRequest("User is not founded");
        }

        var token = _jwtTokenService.GenerateToken(user);
        
        HttpContext.Response.Cookies.Append("token", token);

        return Ok(token);
    }
    
    [HttpPost("SingUp")]
    public async Task<ActionResult<int>> SingUp([FromBody] SingUpRequest request)
    {
        if (await CheckUnique(request.Login, request.Password.Generate()))
            return BadRequest("User already exist");
        
        var user = new User(Guid.NewGuid(), request.Login, request.Password.Generate(), request.Role.ToString());
        return Ok(await _usersService.Create(user));
    }

    private async Task<bool> CheckUnique(string login, string password)
    {
        var users = await _usersService.GetAll();
        return users.Any(x => x.Login.Equals(login) && x.Password.Verify(password));
    }
}