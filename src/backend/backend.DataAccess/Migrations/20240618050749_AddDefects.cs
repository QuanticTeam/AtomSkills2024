using Microsoft.EntityFrameworkCore.Migrations;
using Npgsql.EntityFrameworkCore.PostgreSQL.Metadata;

#nullable disable

namespace backend.DataAccess.Migrations
{
    /// <inheritdoc />
    public partial class AddDefects : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.CreateTable(
                name: "Defects",
                columns: table => new
                {
                    Id = table.Column<int>(type: "integer", nullable: false)
                        .Annotation("Npgsql:ValueGenerationStrategy", NpgsqlValueGenerationStrategy.IdentityByDefaultColumn),
                    Codes = table.Column<string[]>(type: "text[]", nullable: false),
                    Comment = table.Column<string>(type: "text", nullable: false),
                    X1 = table.Column<int>(type: "integer", nullable: true),
                    Y1 = table.Column<int>(type: "integer", nullable: true),
                    X2 = table.Column<int>(type: "integer", nullable: true),
                    Y2 = table.Column<int>(type: "integer", nullable: true),
                    TaskStatusRecordId = table.Column<int>(type: "integer", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Defects", x => x.Id);
                    table.ForeignKey(
                        name: "FK_Defects_TaskStatuses_TaskStatusRecordId",
                        column: x => x.TaskStatusRecordId,
                        principalTable: "TaskStatuses",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateIndex(
                name: "IX_Defects_TaskStatusRecordId",
                table: "Defects",
                column: "TaskStatusRecordId");
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropTable(
                name: "Defects");
        }
    }
}
