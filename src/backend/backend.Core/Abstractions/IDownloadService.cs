namespace backend.Core.Abstractions;

public interface IDownloadService
{
    Task<int> Download();
}