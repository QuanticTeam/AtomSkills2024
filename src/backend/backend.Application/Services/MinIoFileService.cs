using backend.Core.Abstractions;
using backend.Core.Models;
using backend.Core.Options;
using Microsoft.Extensions.Options;
using Minio;
using Minio.DataModel.Args;
using Minio.DataModel.Response;

namespace backend.Application.Services;

public class MinIoFileService : IMinIoFileService
{
    private readonly MinIoOptions _options;
    private readonly IMinioClient _minioClient;

    public MinIoFileService(
        IOptions<MinIoOptions> options,
        IMinioClient minioClient)
    {
        _options = options.Value;
        _minioClient = minioClient;
    }
    
    public async Task<string> GetUrl(string bucketId)
    {
        return await _minioClient.PresignedGetObjectAsync(new PresignedGetObjectArgs().WithBucket(bucketId)).ConfigureAwait(false);
    }
    
    public async Task<int> Upload(MinIoFileModel fileModel)
    {
        try
        {
            var minio = new MinioClient()
                .WithEndpoint(_options.Endpoint)
                .WithCredentials(_options.AccessKey, _options.SecretKey)
                .WithSSL()
                .Build();
            var response = await Run(minio, fileModel.BucketId.ToString(), fileModel.FileName, fileModel.Stream, fileModel.ContentType);
            return response == null ? 0 : 1;
        }
        catch (Exception ex)
        {
            Console.WriteLine(ex.Message);
            return 0;
        }
    }

    private static async Task<PutObjectResponse?> Run(IMinioClient minio, string bucketId, string fileName, Stream fileStream, string contentType)
    {
        var putObjectArgs = new PutObjectArgs()
            .WithBucket(bucketId)
            .WithObject(fileName)
            .WithStreamData(fileStream)
            .WithObjectSize(fileStream.Length)
            .WithContentType(contentType);
        return await minio.PutObjectAsync(putObjectArgs).ConfigureAwait(false);
    }
}