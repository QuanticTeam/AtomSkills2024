using backend.Core.Models;

namespace backend.Core.Abstractions;

public interface IFotosRepository
{
    Task<List<Foto>> Get();
    Task<int> Create(List<Foto> fotos, int statusId);
}