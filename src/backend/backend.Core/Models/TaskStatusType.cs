namespace backend.Core.Models;

public enum TaskStatusType
{
    None,
    Recommended,
    InWork,
    SendToCheck,
    AiVerified,
    Verified,
}