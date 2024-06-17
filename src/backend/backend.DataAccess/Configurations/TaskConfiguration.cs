using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace backend.DataAccess.Configurations;

public class TaskConfiguration : IEntityTypeConfiguration<TaskRecord>
{
    public void Configure(EntityTypeBuilder<TaskRecord> builder)
    {
        builder.HasKey(x => x.Id);

        builder.Property(x => x.Code)
            .IsRequired();

        builder.Property(x => x.Content)
            .IsRequired();

        builder.Property(x => x.Difficult)
            .IsRequired();

        builder.Property(x => x.Time)
            .IsRequired();

        builder.Property(x => x.SupplementKeys)
            .IsRequired();

        builder.Property(x => x.Title)
            .IsRequired();

        builder.HasMany(x => x.LessonRecords)
            .WithMany(l => l.TaskRecords);

        builder.HasMany(x => x.TaskStatusRecords)
            .WithOne(t => t.TaskRecord)
            .HasForeignKey(t => t.TaskRecordId);
    }
}