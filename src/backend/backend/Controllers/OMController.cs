using backend.Contracts;
using backend.Contracts.OM;
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
    public OMController()
    {
        
    }
    
    [Authorize]
    [HttpPost("Tags")]
    public async Task<ActionResult<TraitsResponse>> GetTags()
    {
        return StatusCode(StatusCodes.Status200OK, new List<Trait> {
            new Trait
            {
                Code = "1",
                Name = "ABC",
                Description = "ABC is full"
            },
            new Trait
            {
                Code = "2",
                Name = "OMG",
                Description = "Oh My God!"
            },
            new Trait
            {
                Code = "42",
                Name = "Pepe",
                Description = "PEPEPEPEPE"
            },            
        });
    }

    [Authorize]
    [HttpPost("Topics")]
    public async Task<ActionResult<TopicsResponse>> GetTopics()
    {
        return StatusCode(StatusCodes.Status200OK, new List<Topic> {
            new Topic
            {
                Code = "1",
                Title = "Мастер класс по доению коров",
                Lessons = new List<Lesson> { },
                Traits = new List<Trait> { },
                Description = "Это круто"
            },
            new Topic
            {
                Code = "2",
                Title = "Питон",
                Lessons = new List<Lesson> { },
                Traits = new List<Trait> { },
                Description = "Змею в каждый дом!"
            },
            new Topic
            {
                Code = "43",
                Title = "Просто курс",
                Lessons = new List<Lesson> {},
                Traits = new List<Trait> { },
                Description = "Не подписывайся"
            },            
        });
    }

    private List<Lesson> _lessons = new List<Lesson> 
    {
        new Lesson
        {
            Code = "12412441",
            Title = "Мой первый надой",
            Content = "## MARKDOWN PREVIEW: 01",
            Author = "Выдойка М.Ю.",
            Supplements = new List<string> { "34", "45", "56" },
            Traits = new List<Trait>
            {
                new Trait()
                {
                    Code = "1",
                    Name = "Name1",
                    Description = "Description1"
                },
                new Trait()
                {
                    Code = "2",
                    Name = "Name2",
                    Description = "Description2"
                },
            },
            Tasks = new List<Task>
            {
                new Task()
                {
                    Code = "1",
                    Title = "Title1",
                    Content = "## MARKDOWN PREVIEW: 01",
                    Difficulty = 10,
                    Time = 2,
                    Supplements = new List<string> { "34", "45", "56" },
                },
                new Task()
                {
                    Code = "2",
                    Title = "Title2",
                    Content = "## MARKDOWN PREVIEW: 02",
                    Difficulty = 10,
                    Time = 1,
                    Supplements = new List<string> { "34", "45", "56" },
                }
            },
            Topics = new List<Topic>()
            {
                new Topic()
                {
                    Code = "1",
                    Title = "Title1",
                    Description = "Description1",
                },
                new Topic()
                {
                    Code = "2",
                    Title = "Title2",
                    Description = "Description2",
                }
            }
        },
        new Lesson
        {
            Code = "123",
            Title = "Вяжем вместе",
            Content = "## MARKDOWN PREVIEW: 02",
            Author = "Выдойка М.В.",
            Supplements = new List<string> { "34", "45", "56" },
            Traits = new List<Trait>
            {
                new Trait()
                {
                    Code = "3",
                    Name = "Name3",
                    Description = "Description3"
                },
                new Trait()
                {
                    Code = "4",
                    Name = "Name4",
                    Description = "Description4"
                },
            },
            Tasks = new List<Task>
            {
                new Task()
                {
                    Code = "3",
                    Title = "Title3",
                    Content = "## MARKDOWN PREVIEW: 03",
                    Difficulty = 10,
                    Time = 2,
                    Supplements = new List<string> { "34", "45", "56" },
                },
                new Task()
                {
                    Code = "4",
                    Title = "Title4",
                    Content = "## MARKDOWN PREVIEW: 04",
                    Difficulty = 10,
                    Time = 1,
                    Supplements = new List<string> { "34", "45", "56" },
                }
            },
            Topics = new List<Topic>()
            {
                new Topic()
                {
                    Code = "3",
                    Title = "Title3",
                    Description = "Description3",
                },
                new Topic()
                {
                    Code = "4",
                    Title = "Title4",
                    Description = "Description4",
                }
            }
        },
    };

    [Authorize]
    [HttpPost("Lessons")]
    public async Task<ActionResult<LessonsResponse>> GetLessons(GetSortAndFilterRequest request)
    {
        if (!ValidateFilterType(request))
        {
            return BadRequest();
        }

        var lessons = _lessons.AsQueryable();
        
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
    public async Task<ActionResult<Lesson>> GetLesson(LessonRequest request)
    {
        var lesson = _lessons.LastOrDefault(t => t.Code == request.Code);

        if (lesson == null)
            return NoContent();

        return StatusCode(StatusCodes.Status200OK, lesson);
    }

    private List<Task> _tasks = new List<Task> {
            new Task
            {
                Code = "12",
                Title = "Подход к корове №2",
                Content = "Попробуй взять советский мощный ...",
                Supplements = new List<string> { "123" },
                Difficulty = 4,
                Time = 15,
            },
        }; 

    [Authorize]
    [HttpPost("Tasks")]
    public async Task<ActionResult<TasksResponse>> GetTasks(GetSortAndFilterRequest request)
    {
        if (!ValidateFilterType(request))
        {
            return BadRequest();
        }

        return StatusCode(StatusCodes.Status200OK, _tasks);
    }

    [Authorize]
    [HttpPost("Task")]
    public async Task<ActionResult<Task>> GetTask(string taskCode)
    {
        var task = _tasks.LastOrDefault(t => t.Code == taskCode);

        if (task == null)
            return NoContent();

        return StatusCode(StatusCodes.Status200OK, task);
    }
}