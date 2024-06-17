using backend.Core.Models;

namespace backend.Core.Abstractions;

public interface ILessonsRepository
{
    Task<List<Lesson>> Get();
    Task<int> Create(Lesson lesson);
    Task<int> Update(Lesson lesson);
    Task<int> Delete(string code);
}