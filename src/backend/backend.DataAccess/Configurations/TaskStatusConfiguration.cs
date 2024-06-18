using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace backend.DataAccess.Configurations;

public class TaskStatusConfiguration : IEntityTypeConfiguration<TaskStatusRecord>
{
    public void Configure(EntityTypeBuilder<TaskStatusRecord> builder)
    {
        builder.HasKey(x => x.Id);

        builder.Property(x => x.Status)
            .IsRequired();

        builder.Property(x => x.AutomationSystemStatus)
            .IsRequired();

        builder.Property(x => x.StartedAt)
            .IsRequired();

        builder.Property(x => x.FinishedAt);

        builder.Property(x => x.Mark);

        builder.HasMany(x => x.RecommendationRecords)
            .WithOne(r => r.TaskStatusRecord)
            .HasForeignKey(r => r.TaskStatusRecordId);
        
        builder.HasMany(x => x.DefectRecords)
            .WithOne(r => r.TaskStatusRecord)
            .HasForeignKey(r => r.TaskStatusRecordId);
        
        builder.HasMany(x => x.Fotos)
            .WithOne(r => r.TaskStatusRecord)
            .HasForeignKey(r => r.TaskStatusRecordId);
    }
    
}