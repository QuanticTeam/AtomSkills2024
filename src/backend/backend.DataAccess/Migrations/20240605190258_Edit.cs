using System;
using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace backend.DataAccess.Migrations
{
    /// <inheritdoc />
    public partial class Edit : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropColumn(
                name: "FileKey",
                table: "Somethings");

            migrationBuilder.AddColumn<string[]>(
                name: "FileKeys",
                table: "Somethings",
                type: "text[]",
                nullable: false,
                defaultValue: new string[0]);
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropColumn(
                name: "FileKeys",
                table: "Somethings");

            migrationBuilder.AddColumn<Guid>(
                name: "FileKey",
                table: "Somethings",
                type: "uuid",
                nullable: false,
                defaultValue: new Guid("00000000-0000-0000-0000-000000000000"));
        }
    }
}
