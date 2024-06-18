using backend.Core.Models;

namespace backend.Core.Abstractions;

public interface ILessonsService
{
    Task<List<Lesson>> GetAll();
    Task<Lesson?> Get(string code);
}