using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;

namespace backend.DataAccess;

public class BackendDbContext : DbContext
{
    public BackendDbContext(DbContextOptions<BackendDbContext> options) : base(options)
    {
    }
    
    public DbSet<SomethingRecord> Somethings { get; set; }
    
    public DbSet<UserRecord> Users { get; set; }
    
    public DbSet<LessonRecord> Lessons { get; set; }
    
    public DbSet<TaskRecord> Tasks { get; set; }
    
    public DbSet<TopicRecord> Topics { get; set; }
    
    public DbSet<TraitRecord> Traits { get; set; }
    
    public DbSet<TaskStatusRecord> TaskStatuses { get; set; }
    
    public DbSet<RecommendationRecord> Recommendations { get; set; }
}