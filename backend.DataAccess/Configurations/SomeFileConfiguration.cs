using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace backend.DataAccess.Configurations;

public class SomeFileConfiguration : IEntityTypeConfiguration<SomeFileRecord>
{
    public void Configure(EntityTypeBuilder<SomeFileRecord> builder)
    {
        builder.HasKey(x => x.Id);
        
        builder.Property(x => x.Key)
            .IsRequired();

        builder.Property(x => x.Name)
            .IsRequired();

        builder.Property(x => x.ContentType)
            .IsRequired();

        builder.Property(x => x.Content)
            .IsRequired();
    }
}