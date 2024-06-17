using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace backend.DataAccess.Configurations;

public class SomethingConfiguration : IEntityTypeConfiguration<SomethingRecord>
{
    public void Configure(EntityTypeBuilder<SomethingRecord> builder)
    {
        builder.HasKey(x => x.Id);
        
        builder.Property(x => x.Key)
            .IsRequired();

        builder.Property(x => x.Name)
            .IsRequired();

        builder.Property(x => x.Number)
            .IsRequired();

        builder.Property(x => x.Integer)
            .IsRequired();
        
        builder.Property(x => x.DateTime)
            .IsRequired();

        builder.Property(x => x.FileKeys)
            .IsRequired();
    }
}