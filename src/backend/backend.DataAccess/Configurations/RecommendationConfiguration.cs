using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace backend.DataAccess.Configurations;

public class RecommendationConfiguration : IEntityTypeConfiguration<RecommendationRecord>
{
    public void Configure(EntityTypeBuilder<RecommendationRecord> builder)
    {
        builder.HasKey(x => x.Id);

        builder.Property(x => x.Text)
            .IsRequired();

        builder.Property(x => x.FileKeys)
            .IsRequired();
    }
}