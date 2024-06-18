using backend.Contracts;
using backend.Contracts.OM;
using backend.Core.Abstractions;
using backend.Core.Models;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Task = backend.Core.Models.Task;

namespace backend.Controllers;

[ApiController]
[Route("[controller]")]
public class StatusController : ControllerBase
{
    [Authorize]
    [HttpPost("TakeTaskInWork")]
    public async Task<ActionResult<int>> TakeTaskInWork(
        [FromServices] ITasksService tasksService,
        TaskRequest request)
    {
        if (HttpContext.User.IsInRole("Mentor"))
            return StatusCode(StatusCodes.Status403Forbidden, new 
            {
                Code = "INCORRECT_USER",
                Message = "Incorrect user, must be student",
            }); 
        
        var userId = HttpContext.User.Claims.FirstOrDefault(x => x.Type.Equals("userId"))?.Value ?? string.Empty;

        var result = await tasksService.TakeTaskInWork(request.Code, userId);

        return StatusCode(result == 0 ? StatusCodes.Status404NotFound : StatusCodes.Status200OK, result);
    }
    
    [Authorize]
    [HttpPost("SendTaskToCheck")]
    public async Task<ActionResult<Task>> SendTaskToCheck(
        [FromServices] ITasksService tasksService,
        SendTaskToCheckRequest request)
    {
        if (HttpContext.User.IsInRole("Mentor"))
            return StatusCode(StatusCodes.Status403Forbidden, new 
            {
                Code = "INCORRECT_USER",
                Message = "Incorrect user, must be student",
            });
        
        var fotos = request.FileKeyAndDescriptions
            .Select(x => new Foto
            {
                Key = x.FileKey,
                Comment = x.Description,
            })
            .ToList();

        var result = await tasksService.SendTaskToCheck(request.TaskStatusId, fotos);

        return StatusCode(result == 0 ? StatusCodes.Status404NotFound : StatusCodes.Status200OK, result);
    }
    
    [Authorize]
    [HttpPost("VerifiedTask")]
    public async Task<ActionResult<Task>> VerifiedTask(
        [FromServices] ITasksService tasksService,
        VerifiedTaskRequest request)
    {
        if (HttpContext.User.IsInRole("Student"))
        {
            return StatusCode(StatusCodes.Status403Forbidden, new 
            {
                Code = "INCORRECT_USER",
                Message = "Incorrect user, must be mentor",
            });
        }

        var defects = request.Defects.Select(x => new Defect
        {
            FileKey = x.FileKey,
            Codes = x.Codes.ToArray(),
            Comment = x.Comment,
            X1 = x.X1,
            Y1 = x.Y1,
            X2 = x.X2,
            Y2 = x.Y2,
            TaskStatusRecordId = request.TaskId,
        }).ToList();

        var result = await tasksService.VerifiedTask(defects, request.TaskId, request.Mark);

        return StatusCode(result == 0 ? StatusCodes.Status404NotFound : StatusCodes.Status200OK, result);
    }
    
    [Authorize]
    [HttpPost("RecommendedRework")]
    public async Task<ActionResult<Task>> RecommendedRework(
        [FromServices] ITasksService tasksService,
        int TaskId)
    {
        if (HttpContext.User.IsInRole("Student"))
        {
            return StatusCode(StatusCodes.Status403Forbidden, new 
            {
                Code = "INCORRECT_USER",
                Message = "Incorrect user, must be mentor",
            });
        }

        var result = await tasksService.RecommendedRework(TaskId);

        return StatusCode(result == 0 ? StatusCodes.Status404NotFound : StatusCodes.Status200OK, result);
    }
    
    [Authorize]
    [HttpPost("GetTaskStatuses")]
    public async Task<ActionResult<List<Core.Models.TaskStatus>>> GetTaskStatuses(
        [FromServices] ITasksService tasksService,
        TaskRequest request)
    {
        var userId = HttpContext.User.Claims.FirstOrDefault(x => x.Type.Equals("userId"))?.Value ?? string.Empty;

        var taskStatuses = await tasksService.GetTaskStatuses(request.Code, userId);

        return StatusCode(StatusCodes.Status200OK, taskStatuses);
    }
    
    [Authorize]
    [HttpPost("GetTaskStatus")]
    public async Task<ActionResult<List<Core.Models.TaskStatus>>> GetTaskStatus(
        [FromServices] ITasksService tasksService,
        TaskStatusRequest request)
    {
        var taskStatuses = await tasksService.GetTaskStatus(request.Id);

        return StatusCode(StatusCodes.Status200OK, taskStatuses);
    }
}