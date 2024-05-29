using backend.Core.Models;

namespace backend.Core.Abstractions;

public interface IMinIoFileService
{
    Task<(MemoryStream, string)> Download(string fileName);
    Task<int> Upload(MinIoFileModel fileModel);
}