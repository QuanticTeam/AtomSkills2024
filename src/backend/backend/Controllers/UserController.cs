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

    [HttpGet("GetRoles")]
    public ActionResult<List<EnumResponse>> GetRoles()
    {
        return EnumExtension.GetEnumValues<UserRole>();
    }

    [HttpPost("CheckLogin")]
    public async Task<ActionResult<string>> CheckLogin([FromBody] CheckLoginRequest request)
    {
        var users = await _usersService.GetAll();
        var user = users.FirstOrDefault(x => x.Login.Equals(request.Login, StringComparison.InvariantCultureIgnoreCase));

        if (user == null) {
            return StatusCode(StatusCodes.Status200OK, new { Taken = false });
        }

        return StatusCode(StatusCodes.Status200OK, new { Taken = true });
    }


    [HttpPost("Login")]
    public async Task<ActionResult<string>> Login([FromBody] LoginRequest request)
    {
        var users = await _usersService.GetAll();
        var user = users.FirstOrDefault(x => x.Login.Equals(request.Login, StringComparison.InvariantCultureIgnoreCase)
            && x.Password.Verify(request.Password));

        if (user == null)
        {
            return StatusCode(StatusCodes.Status403Forbidden, new {
                Code = "LOGIN_WRONG_CREDENTIALS",
                Message = "Incorrect username or password",
            });
        }

        var token = _jwtTokenService.GenerateToken(user);

        HttpContext.Response.Cookies.Append("token", token);

        return Ok(token);
    }

    [HttpPost("Register")]
    public async Task<ActionResult<int>> SignUp([FromBody] SignUpRequest request)
    {
        if (await CheckUnique(request.Login))
            return StatusCode(StatusCodes.Status409Conflict, new {
                Code = "REGISTER_USERNAME_TAKEN",
                Message = "Username is taken",
            });
        
        var user = new User(
            Guid.NewGuid(), 
            request.Login, 
            request.Password.Obfuscate(), 
            request.Role.ToString(),
            request.FirstName,
            request.MiddleName,
            request.LastName,
            request.Email,
            request.Phone);
        
        return Ok(await _usersService.Create(user));
    }

    [HttpPost("Update")]
    public async Task<ActionResult<int>> Update([FromBody] UpdateUserRequest request)
    {
        var user = await _usersService.Get(Guid.Parse(request.Key));
        
        if (user == null)
            return StatusCode(StatusCodes.Status404NotFound, new {
                Code = "USER_UPDATE_NOT_FOUND",
                Message = "User not found",
            });
        
        var updateUser = new User(
            user.Key, 
            request.Login, 
            request.Password.Obfuscate(), 
            request.Role.ToString(),
            request.FirstName,
            request.MiddleName,
            request.LastName,
            request.Email,
            request.Phone);
        
        return Ok(await _usersService.Update(updateUser));
    }

    [HttpPost("Delete")]
    public async Task<ActionResult<int>> Delete([FromBody] Guid userKey)
    {
        return Ok(await _usersService.Delete(userKey));
    }

    private async Task<bool> CheckUnique(string login)
    {
        var users = await _usersService.GetAll();
        return users.Any(x => x.Login.Equals(login, StringComparison.InvariantCultureIgnoreCase));
    }
}
