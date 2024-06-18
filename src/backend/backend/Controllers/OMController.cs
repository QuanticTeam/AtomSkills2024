using backend.Contracts;
using backend.Contracts.OM;
using backend.Core.Abstractions;
using backend.Core.Models;
using backend.Extensions;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Task = backend.Core.Models.Task;

namespace backend.Controllers;

[ApiController]
[Route("[controller]")]
public class OMController : ControllerBase
{
    [Authorize]
    [HttpPost("Tags")]
    public async Task<ActionResult<TraitsResponse>> GetTags([FromServices] ITraitRepository traitRepository)
    {
        var traits = await traitRepository.Get();
        return StatusCode(StatusCodes.Status200OK, traits);
    }

    [Authorize]
    [HttpPost("Topics")]
    public async Task<ActionResult<TopicsResponse>> GetTopics([FromServices] ITopicsRepository topicsRepository)
    {
        var topics = await topicsRepository.Get();
        return StatusCode(StatusCodes.Status200OK, topics);
    }

    [Authorize]
    [HttpPost("Lessons")]
    public async Task<ActionResult<LessonsResponse>> GetLessons(
        [FromServices] ILessonsRepository lessonsRepository,
        GetSortAndFilterRequest request)
    {
        if (!ValidateFilterType(request))
        {
            return BadRequest();
        }

        var lessons = (await lessonsRepository.Get()).AsQueryable();
        
        if (request.Filters.Any())
        {
            lessons = lessons.Where(FilterExtension.Filter<Lesson>(request.Filters));
        }

        if (!string.IsNullOrEmpty(request.OrderBy))
        {
            lessons = lessons.Order(request.OrderBy, request.Descending);
        } 

        return StatusCode(StatusCodes.Status200OK, lessons);
    }

    private bool ValidateFilterType(GetSortAndFilterRequest request)
    {
        foreach (var filter in request.Filters)

        switch (filter.FilterType)
        {
            case FilterType.Contains:
            case FilterType.Equals:
            case FilterType.GreaterThan:
            case FilterType.LessThan:
            {
                if (filter.Values.Count() != 1)
                    return false;
                break;
            }
            default: throw new NotImplementedException($"Нет имплементации для типа фильтра: {filter.FilterType}");
        }

        return true;
    }

    [Authorize]
    [HttpPost("Lesson")]
    public async Task<ActionResult<Lesson>> GetLesson(
        [FromServices] ILessonsRepository lessonsRepository,
        LessonRequest request)
    {
        var lesson = (await lessonsRepository.Get()).FirstOrDefault(t => t.Code == request.Code);

        if (lesson == null)
            return NoContent();

        return StatusCode(StatusCodes.Status200OK, lesson);
    }

    [Authorize]
    [HttpPost("Tasks")]
    public async Task<ActionResult<TasksResponse>> GetTasks(
        [FromServices] ITasksRepository taskRepository,
        GetSortAndFilterRequest request)
    {
        if (!ValidateFilterType(request))
        {
            return BadRequest();
        }

        var tasks = (await taskRepository.Get()).AsQueryable();
        
        if (request.Filters.Any())
        {
            tasks = tasks.Where(FilterExtension.Filter<Task>(request.Filters));
        }

        
        if (!string.IsNullOrEmpty(request.OMCode))
        {
            tasks = tasks.Where(t => t.Lessons.Any(l => l.Code.Equals(request.OMCode)));
        }

        if (!string.IsNullOrEmpty(request.OrderBy))
        {
            tasks = tasks.Order(request.OrderBy, request.Descending);
        } 


        return StatusCode(StatusCodes.Status200OK, tasks);
    }

    [Authorize]
    [HttpPost("Task")]
    public async Task<ActionResult<Task>> GetTask(
        [FromServices] ITasksRepository taskRepository,
        TaskRequest request)
    {
        var task = (await taskRepository.Get()).FirstOrDefault(t => t.Code == request.Code);

        if (task == null)
            return NoContent();

        return StatusCode(StatusCodes.Status200OK, task);
    }

    [Authorize]
    [HttpPost("TaskStatuses")]
    public async Task<ActionResult<TaskStatusesResponse>> GetTaskStatuses(
        ITaskStatusesRepository taskStatusesRepository,
        TaskStatusesRequest request)
    {
        var items = await taskStatusesRepository.Get();
        return Ok(new TaskStatusesResponse
        {
            Items = items.ToList(),
        });
    }


}