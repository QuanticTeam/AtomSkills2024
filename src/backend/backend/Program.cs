using backend.Application.Services;
using backend.Core.Abstractions;
using backend.Core.Options;
using backend.DataAccess;
using backend.DataAccess.Repositories;
using backend.Extensions;
using backend.Notifications;
using Microsoft.EntityFrameworkCore;
using Microsoft.OpenApi.Models;
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
builder.Services.AddScoped<ISomethingsRepository, SomethingsRepository>();
builder.Services.AddScoped<IUsersRepository, UsersRepository>();
builder.Services.AddScoped<ISomethingsService, SomethingsService>();
builder.Services.AddScoped<IUsersService, UsersService>();
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