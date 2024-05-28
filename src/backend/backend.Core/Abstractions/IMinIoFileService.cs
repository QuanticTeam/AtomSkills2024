using backend.Core.Models;

namespace backend.Core.Abstractions;

public interface IMinIoFileService
{
    Task<string> GetUrl(string bucketId);
    Task<int> Upload(MinIoFileModel fileModel);
}