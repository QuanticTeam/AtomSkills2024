using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace backend.DataAccess.Configurations;

public class FotoConfiguration : IEntityTypeConfiguration<FotoRecord>
{
    public void Configure(EntityTypeBuilder<FotoRecord> builder)
    {
        builder.HasKey(x => x.Id);

        builder.Property(x => x.Comment)
            .IsRequired();

        builder.Property(x => x.FotoKey)
            .IsRequired();
    }
}