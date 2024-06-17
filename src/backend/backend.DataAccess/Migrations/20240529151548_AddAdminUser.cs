using backend.Core.Models;
using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace backend.DataAccess.Migrations
{
    /// <inheritdoc />
    public partial class AddAdminUser : Migration
    {
        private static readonly string[] columns = new []{ "Key", "Login", "Password", "Role" };

        private static readonly object[] values = new object[] { Guid.NewGuid(), "admin", BCrypt.Net.BCrypt.EnhancedHashPassword("admin"), UserRole.Admin.ToString() };

        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.InsertData(
                table: "Users", 
                columns: columns,
                values: values);
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DeleteData(
                table: "Users", 
                keyColumn: columns.First(),
                keyColumnType: "Guid",
                keyValue: values.First());
        }
    }
}
