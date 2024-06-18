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
    [HttpGet("GetTaskCodes")]
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

        var filtered = tasks
            .Where(x => x.TaskStatuses.Exists(t => t.Status.Equals(TaskStatusType.Verified.ToString()) || t.Status.Equals(TaskStatusType.Recommended.ToString())))
            .OrderByDescending(x => x.TaskStatuses.Count(t => t.Mark == 2))
            .ThenByDescending(x => x.TaskStatuses.Count(t => t.Mark == 3))
            .ThenByDescending(x => x.TaskStatuses.Count(t => t.Mark == 4))
            .ThenByDescending(x => x.TaskStatuses.Count(t => t.Mark == 5));
        
        return new RankTasksResponse
        {
            RankTasks = filtered.Select(x => new RankTask
            {
                Code = x.Code,
                Title = x.Title,
                CountTwo = x.TaskStatuses.Count(t => t.Mark == 2),
                CountThree = x.TaskStatuses.Count(t => t.Mark == 3),
                CountFour = x.TaskStatuses.Count(t => t.Mark == 4),
                CountFive = x.TaskStatuses.Count(t => t.Mark == 5)
            }).ToList()
        };
    }
}