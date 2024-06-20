using backend.Contracts.Rank;
using backend.Core.Abstractions;
using backend.Core.Models;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using TaskStatus = backend.Core.Models.TaskStatus;

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
            .Exists(t => t.TaskCode.Equals(request.Code) && (t.Status.Equals(TaskStatusType.Verified.ToString()) || t.Status.Equals(TaskStatusType.Recommended.ToString()))))
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
            .ToList();

        var result = new List<RankTask>();

        foreach (var task in tasks)
        {
            var ts = task.TaskStatuses
                .Where(ts => ts.FinishedAt.HasValue && ts.Mark.HasValue &&
                    (ts.Status.Equals(TaskStatusType.Verified.ToString()) || ts.Status.Equals(TaskStatusType.Recommended.ToString())))
                .ToList();

            int rank = 0;
            if (ts.Any())
                rank = (int) (1000 * (ts.Sum(t => t.Mark!.Value) / ts.Count()) - ts.Select(t => t.FinishedAt!.Value.Subtract(t.StartedAt).TotalMinutes).Max());

            result.Add(new RankTask
            {
                Rank = rank,
                Code = task.Code,
                Title = task.Title,
                CountTwo = task.TaskStatuses.Count(t => t.Mark == 2),
                CountThree = task.TaskStatuses.Count(t => t.Mark == 3),
                CountFour = task.TaskStatuses.Count(t => t.Mark == 4),
                CountFive = task.TaskStatuses.Count(t => t.Mark == 5)
            });
        }

        return new RankTasksResponse
        {
            RankTasks = result.OrderByDescending(r => r.Rank).ToList(),
        };
    }

    [Authorize]
    [HttpPost("Test")]
    public async Task<ActionResult<RankStudentsResponse>> RankStudents(
        [FromServices] ITasksRepository tasksRepository,
        [FromServices] ITaskStatusesRepository taskStatusesRepository,
        RankStudentsRequest request)
    {
        var task = await tasksRepository.GetByCode(request.TaskCode);

        if (task == null)
            return BadRequest($"Task not found by code: {request.TaskCode}");

        var taskStatuses = await taskStatusesRepository.Get();

        return Ok();

        // var filtered = taskStatuses.Where(
        //     ts => (ts.Status.Equals(TaskStatusType.Verified.ToString())
        //             || ts.Status.Equals(TaskStatusType.Recommended.ToString()))
        //         && ts.Mark.HasValue && ts.Mark.Value > 2);

        // filtered.GroupBy(ts => ts.UserKey).Select(group => (group.Key, group.))

        // return Ok(new RankStudentsResponse
        // {
        //     filtered
        // });
    }

    private int TaskScore(TaskStatus taskStatus)
    {
        var diff = taskStatus.FinishedAt!.Value.Subtract(taskStatus.StartedAt);
        return 1000 * taskStatus.Mark!.Value - (int) diff.TotalMinutes;
    }
}
