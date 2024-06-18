using backend.DataAccess.Entities;
using Task = backend.Core.Models.Task;

namespace backend.Extensions;

public static class ModelsExtension
{
    public static Task ToModel(this TaskRecord record)
    {
        return new Task
        {
            Code = record.Code,

        };
    }
}
