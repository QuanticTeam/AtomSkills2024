using backend.Core.Abstractions;
using backend.Core.Models;

namespace backend.Application.Services;

public class LessonsService : ILessonsService
{
    private readonly ILessonsRepository _lessonsRepository;

    public LessonsService(ILessonsRepository lessonsRepository)
    {
        _lessonsRepository = lessonsRepository;
    }
    
    public async Task<List<Lesson>> GetAll()
    {
        return await _lessonsRepository.Get();
    }

    public async Task<Lesson?> Get(string code)
    {
        var orders = await _lessonsRepository.Get();
        return orders.FirstOrDefault(x => x.Code.Equals(code));
    }
}