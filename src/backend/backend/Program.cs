using backend.Application.Services;
using backend.Core.Abstractions;
using backend.Core.Options;
using backend.DataAccess;
using backend.DataAccess.Repositories;
using backend.Extensions;
using backend.Notifications;
using Microsoft.EntityFrameworkCore;
using Minio;

var builder = WebApplication.CreateBuilder(args);

builder.Services.AddControllers();
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGenAuth();
builder.Services.AddDbContext<BackendDbContext>(
    options =>
    {
        options.UseNpgsql(builder.Configuration["ConnectionString"]);
    });

// repositories
builder.Services.AddScoped<ISomethingsRepository, SomethingsRepository>();
builder.Services.AddScoped<IUsersRepository, UsersRepository>();
builder.Services.AddScoped<IGeneralRepository, GeneralRepository>();
builder.Services.AddScoped<ILessonsRepository, LessonsRepository>();
builder.Services.AddScoped<ITasksRepository, TasksRepository>();
builder.Services.AddScoped<ITopicsRepository, TopicsRepository>();
builder.Services.AddScoped<ITraitRepository, TraitRepository>();
builder.Services.AddScoped<ITaskStatusesRepository, TaskStatusesRepository>();
builder.Services.AddScoped<IRecommendationsRepository, RecommendationsRepository>();
builder.Services.AddScoped<IDefectsRepository, DefectsRepository>();
builder.Services.AddScoped<IFotosRepository, FotosRepository>();

// services
builder.Services.AddScoped<ISomethingsService, SomethingsService>();
builder.Services.AddScoped<IDownloadService, DownloadLessonsService>();
builder.Services.AddScoped<IUsersService, UsersService>();
builder.Services.AddScoped<ILessonsService, LessonsService>();
builder.Services.AddScoped<ITasksService, TasksService>();
builder.Services.AddSignalR();
builder.Services.Configure<JwtTokenOptions>(builder.Configuration.GetSection("JwtOptions"));
builder.Services.AddScoped<IJwtTokenService, JwtTokenService>();
builder.Services.Configure<MinIoOptions>(builder.Configuration.GetSection("MinIoOptions"));
builder.Services.AddMinio(builder.Configuration["MinIoOptions:AccessKey"], builder.Configuration["MinIoOptions:SecretKey"]);
builder.Services.AddMinio(configureClient => configureClient
    .WithEndpoint(builder.Configuration["MinIoOptions:Endpoint"])
    .WithCredentials(builder.Configuration["MinIoOptions:AccessKey"], builder.Configuration["MinIoOptions:SecretKey"])
    .Build());
builder.Services.AddScoped<IMinIoFileService, MinIoFileService>();

# region (Configure ML)
builder.Services.Configure<AiOptions>(builder.Configuration.GetSection("AiOptions"));
builder.Services.Configure<MLOptions>(builder.Configuration.GetSection("MLOptions"));


builder.Services.AddSingleton<IAIClient, AIClient>();
#endregion

// Hosted services
builder.Services.AddHostedService<BackgroundTaskStatusService>();
builder.Services.AddHostedService<BackgroundAiTaskStatusService>();

// Content load
builder.Services.Configure<ContentLoadOptions>(builder.Configuration.GetSection("ContentLoadOptions"));
builder.Services.AddScoped<IContentLoadService, ContentLoadService>();

builder.Services.AddAuth(builder.Configuration);


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

app.UseAuthentication();

app.UseRouting().UseAuthorization().UseEndpoints(endpoints =>
{
    endpoints.MapHub<ToastNotificationHub>("/toastNotificationHub");
});

app.UseHttpsRedirection();

app.MapControllers();

app.Run();