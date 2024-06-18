using backend.Core.JsonModels;
using backend.Core.Models;

namespace backend.Core.Abstractions;

public interface IMinIoFileService
{
    Task<string> GetOriginalFileName(string fileName);
    Task<(MemoryStream, string)> Download(string fileName);
    Task<int> Upload(MinIoFileModel fileModel);
    // Task<List<string>> Upload(List<JsonSupplement> supplements);
    Task<Supplement> Upload(JsonSupplement supplement);
    Task<string> GetOriginalFileNameWithUnescape(string fileName);
    Task<string> GetTitleWithUnescape(string fileName);
}