using backend.Application.Services;
using backend.Core.Abstractions;
using backend.DataAccess;
using backend.DataAccess.Repositories;
using backend.FileUploads;
using backend.Notifications;
using Microsoft.EntityFrameworkCore;
using Minio;

var builder = WebApplication.CreateBuilder(args);

builder.Services.AddControllers();
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen();
builder.Services.AddDbContext<BackendDbContext>(
    options =>
    {
        options.UseNpgsql(builder.Configuration["ConnectionString"]);
    });
builder.Services.AddScoped<ISomethingsRepository, SomethingsRepository>();
builder.Services.AddScoped<ISomeFilesRepository, SomeFilesRepository>();
builder.Services.AddScoped<IUsersRepository, UsersRepository>();
builder.Services.AddScoped<ISomethingsService, SomethingsService>();
builder.Services.AddScoped<ISomeFilesService, SomeFilesService>();
builder.Services.AddScoped<IUsersService, UsersService>();
builder.Services.AddSignalR();
builder.Services.Configure<MinIoOptions>(builder.Configuration.GetSection("MinIoOptions"));
builder.Services.AddMinio(builder.Configuration["MinIoOptions:AccessKey"], builder.Configuration["MinIoOptions:SecretKey"]);
builder.Services.AddMinio(configureClient => configureClient
    .WithEndpoint(builder.Configuration["MinIoOptions:Endpoint"])
    .WithCredentials(builder.Configuration["MinIoOptions:AccessKey"], builder.Configuration["MinIoOptions:SecretKey"])
    .Build());


var app = builder.Build();

using (var scope = app.Services.CreateScope())
{
    var services = scope.ServiceProvider;

    var context = services.GetRequiredService<BackendDbContext>();
    context.Database.Migrate();
}

app.UseCors(x =>
{
    x.WithHeaders().AllowAnyHeader();
    x.WithOrigins("http://localhost:3000");
    x.WithMethods().AllowAnyMethod();
});

if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI();
}

app.UseRouting().UseEndpoints(endpoints =>
{
    endpoints.MapHub<ToastNotificationHub>("/toastNotificationHub");
});

app.UseHttpsRedirection();

app.UseAuthentication();

app.MapControllers();

app.Run();