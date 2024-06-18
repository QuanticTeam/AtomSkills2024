using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace backend.DataAccess.Configurations;

public class DefectConfiguration : IEntityTypeConfiguration<DefectRecord>
{
    public void Configure(EntityTypeBuilder<DefectRecord> builder)
    {
        builder.HasKey(x => x.Id);

        builder.Property(x => x.FileKey)
            .IsRequired();

        builder.Property(x => x.Codes)
            .IsRequired();

        builder.Property(x => x.Comment)
            .IsRequired();

        builder.Property(x => x.X1);

        builder.Property(x => x.Y1);

        builder.Property(x => x.X2);

        builder.Property(x => x.Y2);
    }

}