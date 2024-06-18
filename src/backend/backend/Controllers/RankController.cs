using backend.Contracts.Rank;
using backend.Core.Abstractions;
using backend.Core.Models;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;

namespace backend.Controllers;

[ApiController]
[Route("[controller]")]
public class RankController : ControllerBase
{
    private readonly IUsersService _usersService;
    private readonly ITasksRepository _tasksService;

    public RankController(IUsersService usersService, ITasksRepository tasksService)
    {
        _usersService = usersService;
        _tasksService = tasksService;
    }
    
    [Authorize]
    [HttpGet("RankTasks")]
    public async Task<List<string>> GetTaskCodes()
    {
        var tasks = await _tasksService.Get();
        return tasks.Select(x => x.Code).ToList();
    }
    
    [Authorize]
    [HttpPost("RankUsers")]
    public async Task<RankUsersResponse> RankUsers(RankUsersRequest request)
    {
        var users = await _usersService.GetAll();
        
        var filtered = users
            .Where(x => x.TaskStatuses
            .Exists(t => t.TaskCode.Equals(request.Code) && t.Status.Equals(TaskStatusType.Verified.ToString())))
            .OrderByDescending(u => u.TaskStatuses.LastOrDefault()!.Mark)
            .ThenBy(u => (u.TaskStatuses.LastOrDefault()!.FinishedAt - u.TaskStatuses.LastOrDefault()!.StartedAt)!.Value.Minutes)
            .ToList();

        return new RankUsersResponse
        {
            UserRanks = filtered.Select(x => new UserRank
            {
                Name = $"{x.LastName} {x.FirstName} {x.MiddleName}",
                Minutes = (x.TaskStatuses.LastOrDefault()!.FinishedAt - x.TaskStatuses.LastOrDefault()!.StartedAt)!.Value.Minutes,
                Mark = x.TaskStatuses.LastOrDefault()?.Mark
            }).ToList(),
        };
    }
    
    [Authorize]
    [HttpPost("RankTasks")]
    public async Task<RankTasksResponse> RankTasks()
    {
        var tasks = await _tasksService.Get();
        
        
        
        return new RankTasksResponse();
    }
}