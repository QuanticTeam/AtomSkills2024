using TaskStatus = backend.Core.Models.TaskStatus;

public class ExtendedTaskStatus : TaskStatus
{
    public ExtendedTaskStatus(TaskStatus ts, string UserFullName)
    {
        this.AutomationSystemStatus = ts.AutomationSystemStatus;
        this.Defects = ts.Defects;
        this.FinishedAt = ts.FinishedAt;
        this.Fotos = ts.Fotos;
        this.Id = ts.Id;
        this.Mark = ts.Mark;
        this.Recommendations = ts.Recommendations;
        this.StartedAt = ts.StartedAt;
        this.Status = ts.Status;
        this.TaskCode = ts.TaskCode;
        this.UserKey = ts.UserKey;

        this.UserFullName = UserFullName;
    }
    public string UserFullName { get; set; }
}
