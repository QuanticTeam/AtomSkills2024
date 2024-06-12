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
}