using backend.DataAccess.Entities;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace backend.DataAccess.Configurations;

public class UserConfiguration: IEntityTypeConfiguration<UserRecord>
{
    public void Configure(EntityTypeBuilder<UserRecord> builder)
    {
        builder.HasKey(x => x.Id);
        
        builder.Property(x => x.Key)
            .IsRequired();

        builder.Property(x => x.Login)
            .IsRequired();

        builder.Property(x => x.Password)
            .IsRequired();

        builder.Property(x => x.Role)
            .IsRequired();

        builder.Property(x => x.FirstName);
        
        builder.Property(x => x.MiddleName);
        
        builder.Property(x => x.LastName);
        
        builder.Property(x => x.Email);
        
        builder.Property(x => x.Phone);
    }
}