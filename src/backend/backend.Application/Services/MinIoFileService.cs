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
    private const string BucketName = "backet";
    private const string OriginalFileNameKey = "original-file-name";

    public MinIoFileService(
        IOptions<MinIoOptions> options)
    {
        _options = options.Value;
        
        var minio = new MinioClient()
            .WithEndpoint(_options.Endpoint)
            .WithCredentials(_options.AccessKey, _options.SecretKey)
            .Build();
        var found = minio.BucketExistsAsync(new BucketExistsArgs().WithBucket(BucketName)).GetAwaiter().GetResult();
        if (!found)
        {
            minio.MakeBucketAsync(new MakeBucketArgs().WithBucket(BucketName));
        }
    }

    public async Task<string> GetOriginalFileName(string fileName)
    {
        try
        {
            var minio = new MinioClient()
                .WithEndpoint(_options.Endpoint)
                .WithCredentials(_options.AccessKey, _options.SecretKey)
                .Build();
            
            var stat = await minio.StatObjectAsync(new StatObjectArgs().WithObject(fileName).WithBucket(BucketName));

            return stat.MetaData[OriginalFileNameKey];

        }
        catch (Exception ex)
        {
            Console.WriteLine(ex.Message);
            return string.Empty;
        }
    }
    
    public async Task<(MemoryStream, string)> Download(string fileName)
    {
        try
        {
            var minio = new MinioClient()
                .WithEndpoint(_options.Endpoint)
                .WithCredentials(_options.AccessKey, _options.SecretKey)
                .Build();
            
            var stat = await minio.StatObjectAsync(new StatObjectArgs().WithObject(fileName).WithBucket(BucketName));
            
            var downloadStream = new MemoryStream();
            
            var getObjectArgs = new GetObjectArgs()
                .WithBucket(BucketName)
                .WithObject(stat.ObjectName)
                .WithCallbackStream(x =>
                {
                    x.CopyTo(downloadStream);
                    downloadStream.Seek(0, SeekOrigin.Begin);
                });
            
            _ = await minio.GetObjectAsync(getObjectArgs);

            return (downloadStream, stat.MetaData[OriginalFileNameKey]);
        }
        catch (Exception ex)
        {
            Console.WriteLine(ex.Message);
            throw;
        }
    }
    
    public async Task<int> Upload(MinIoFileModel fileModel)
    {
        try
        {
            var minio = new MinioClient()
                .WithEndpoint(_options.Endpoint)
                .WithCredentials(_options.AccessKey, _options.SecretKey)
                .Build();
            var response = await Run(minio, BucketName, fileModel.FileName, fileModel.Stream, fileModel.ContentType, fileModel.MetaData);
            return response == null ? 0 : 1;
        }
        catch (Exception ex)
        {
            Console.WriteLine(ex.Message);
            return 0;
        }
    }

    private static async Task<PutObjectResponse?> Run(
        IMinioClient minio, 
        string bucket, 
        string fileName, 
        Stream fileStream, 
        string contentType, 
        IDictionary<string, string> metaData)
    {
        var putObjectArgs = new PutObjectArgs()
            .WithBucket(bucket)
            .WithObject(fileName)
            .WithStreamData(fileStream)
            .WithObjectSize(fileStream.Length)
            .WithContentType(contentType)
            .WithHeaders(metaData);
        return await minio.PutObjectAsync(putObjectArgs);
    }
}