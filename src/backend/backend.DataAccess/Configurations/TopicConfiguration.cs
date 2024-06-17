using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace backend.DataAccess.Configurations;

public class TopicConfiguration : IEntityTypeConfiguration<TopicRecord>
{
    public void Configure(EntityTypeBuilder<TopicRecord> builder)
    {
        builder.HasKey(x => x.Id);

        builder.Property(x => x.Code)
            .IsRequired();

        builder.Property(x => x.Description)
            .IsRequired();

        builder.Property(x => x.Title)
            .IsRequired();

        builder.HasMany(x => x.LessonRecords)
            .WithMany(l => l.TopicRecords);

        builder.HasMany(x => x.TraitRecords)
            .WithMany(t => t.TopicRecords);
    }
}