using Microsoft.AspNetCore.SignalR;

namespace backend.Notifications;

public class ToastNotificationHub : Hub
{
    public async Task SendNotification(string message)
    {
        await Clients.All.SendAsync("ToastNotification", message);
    }
}