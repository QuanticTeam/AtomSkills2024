using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace backend.DataAccess.Configurations;

public class LessonConfiguration : IEntityTypeConfiguration<LessonRecord>
{
    public void Configure(EntityTypeBuilder<LessonRecord> builder)
    {
        builder.HasKey(x => x.Id);

        builder.Property(x => x.Code)
            .IsRequired();

        builder.Property(x => x.Title)
            .IsRequired();

        builder.Property(x => x.Author)
            .IsRequired();

        builder.Property(x => x.Content)
            .IsRequired();

        builder.Property(x => x.SupplementKeys)
            .IsRequired();

        builder.HasMany(x => x.TaskRecords)
            .WithMany(t => t.LessonRecords);

        builder.HasMany(x => x.TraitRecords)
            .WithMany(t => t.LessonRecords);

    }
    
}