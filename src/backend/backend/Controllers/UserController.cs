using backend.Contracts;
using backend.Core.Abstractions;
using backend.Core.Models;
using backend.Extensions;
using backend.Notifications;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.SignalR;
using TaskStatus = backend.Core.Models.TaskStatus;

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
        return StatusCode(StatusCodes.Status200OK, userId);
    }

    [HttpGet("GetRoles")]
    public ActionResult<List<EnumResponse>> GetRoles()
    {
        return StatusCode(StatusCodes.Status200OK, EnumExtension.GetEnumValues<UserRole>());
    }
    
    [HttpGet("GetUser")]
    public async Task<ActionResult<User>> GetUser()
    {
        var userId = HttpContext.User.Claims.FirstOrDefault(x => x.Type.Equals("userId"))?.Value ?? string.Empty;
        var success = Guid.TryParse((ReadOnlySpan<char>)userId, out var key);
        
        if (!success)
            return StatusCode(StatusCodes.Status403Forbidden, new 
            {
                Code = "SIGN_IN_WRONG_CREDENTIALS",
                Message = "Incorrect login or password",
            });
        
        var user = await _usersService.Get(key);
        
        return StatusCode(StatusCodes.Status200OK, user);
    }
    
    [HttpGet("GetUsers")]
    public async Task<ActionResult<List<User>>> GetUsers()
    {
        var users = await _usersService.GetAll();
        
        return StatusCode(StatusCodes.Status200OK, users);
    }
    
    [HttpGet("GetUsersByRole")]
    public async Task<ActionResult<List<User>>> GetUsersByRole()
    {
        var userRole = HttpContext.User.Claims.FirstOrDefault(x => x.Type.Equals("role"))?.Value ?? string.Empty;
        
        var users = await _usersService.GetAll();
        
        return StatusCode(StatusCodes.Status200OK, users.Where(x => x.Role.Equals(userRole)).ToList());
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


    [HttpPost("SignIn")]
    public async Task<ActionResult<string>> SignIn([FromBody] SignInRequest request)
    {
        var users = await _usersService.GetAll();
        var user = users.FirstOrDefault(x => x.Login.Equals(request.Login, StringComparison.InvariantCultureIgnoreCase)
            && x.Password.Verify(request.Password));

        if (user == null)
        {
            return StatusCode(StatusCodes.Status403Forbidden, new {
                Code = "SIGN_IN_WRONG_CREDENTIALS",
                Message = "Incorrect login or password",
            });
        }

        var token = _jwtTokenService.GenerateToken(user);

        HttpContext.Response.Cookies.Append("token", token);

        return StatusCode(StatusCodes.Status200OK, token);
    }

    [HttpPost("SignUp")]
    public async Task<ActionResult<int>> SignUp([FromBody] SignUpRequest request)
    {
        if (await CheckUnique(request.Login))
            return StatusCode(StatusCodes.Status409Conflict, new {
                Code = "SIGN_UP_LOGIN_TAKEN",
                Message = "Login is taken",
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
            request.Phone,
            new List<TaskStatus>());
        
        return StatusCode(StatusCodes.Status200OK, await _usersService.Create(user));
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
            request.Phone,
            user.TaskStatuses);
        
        return StatusCode(StatusCodes.Status200OK, await _usersService.Update(updateUser));
    }

    [HttpPost("Delete")]
    public async Task<ActionResult<int>> Delete([FromBody] Guid userKey)
    {
        return StatusCode(StatusCodes.Status200OK, await _usersService.Delete(userKey));
    }

    private async Task<bool> CheckUnique(string login)
    {
        var users = await _usersService.GetAll();
        return users.Any(x => x.Login.Equals(login, StringComparison.InvariantCultureIgnoreCase));
    }
}
