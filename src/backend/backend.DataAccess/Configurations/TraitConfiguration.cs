using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace backend.DataAccess.Configurations;

public class TraitConfiguration : IEntityTypeConfiguration<TraitRecord>
{
    public void Configure(EntityTypeBuilder<TraitRecord> builder)
    {
        builder.HasKey(x => x.Id);

        builder.Property(x => x.Code)
            .IsRequired();

        builder.Property(x => x.Name)
            .IsRequired();

        builder.Property(x => x.Description)
            .IsRequired();

        builder.HasMany(x => x.LessonRecords)
            .WithMany(l => l.TraitRecords);

        builder.HasMany(x => x.TopicRecords)
            .WithMany(t => t.TraitRecords);
    }
}