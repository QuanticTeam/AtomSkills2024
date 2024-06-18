using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace backend.DataAccess.Migrations
{
    /// <inheritdoc />
    public partial class AddFileKeyToDefect : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.AddColumn<string>(
                name: "FileKey",
                table: "Defects",
                type: "text",
                nullable: false,
                defaultValue: "");
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropColumn(
                name: "FileKey",
                table: "Defects");
        }
    }
}
