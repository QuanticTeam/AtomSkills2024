using Minio;
using Minio.DataModel.Args;
using Minio.DataModel.Response;

namespace backend.FileUploads;

internal static class MinIoFileUpload
{
    internal static async Task<int> Upload(MinIoOptions options, MinIoFileModel minIoFileModel)
    {
        try
        {
            var minio = new MinioClient()
                .WithEndpoint(options.Endpoint)
                .WithCredentials(options.AccessKey, options.SecretKey)
                .WithSSL()
                .Build();
            var response = await Run(minio, minIoFileModel);
            return response == null ? 0 : 1;
        }
        catch (Exception ex)
        {
            Console.WriteLine(ex.Message);
            return 0;
        }
    }

    private static async Task<PutObjectResponse?> Run(IMinioClient minio, MinIoFileModel minIoFileModel)
    {
        var putObjectArgs = new PutObjectArgs()
            .WithBucket(minIoFileModel.BucketId.ToString())
            .WithObject(minIoFileModel.File.FileName)
            .WithFileName(minIoFileModel.File.Name)
            .WithContentType(minIoFileModel.File.ContentType);
        return await minio.PutObjectAsync(putObjectArgs).ConfigureAwait(false);
    }
}