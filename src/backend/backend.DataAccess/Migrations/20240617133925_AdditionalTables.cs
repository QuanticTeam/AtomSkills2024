using System;
using Microsoft.EntityFrameworkCore.Migrations;
using Npgsql.EntityFrameworkCore.PostgreSQL.Metadata;

#nullable disable

namespace backend.DataAccess.Migrations
{
    /// <inheritdoc />
    public partial class AdditionalTables : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.CreateTable(
                name: "TaskStatuses",
                columns: table => new
                {
                    Id = table.Column<int>(type: "integer", nullable: false)
                        .Annotation("Npgsql:ValueGenerationStrategy", NpgsqlValueGenerationStrategy.IdentityByDefaultColumn),
                    Status = table.Column<string>(type: "text", nullable: false),
                    AutomationSystemStatus = table.Column<string>(type: "text", nullable: false),
                    StartedAt = table.Column<DateTime>(type: "timestamp with time zone", nullable: false),
                    FinishedAt = table.Column<DateTime>(type: "timestamp with time zone", nullable: false),
                    Mark = table.Column<int>(type: "integer", nullable: true),
                    FotoKeys = table.Column<string[]>(type: "text[]", nullable: false),
                    UserRecordId = table.Column<int>(type: "integer", nullable: false),
                    TaskRecordId = table.Column<int>(type: "integer", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_TaskStatuses", x => x.Id);
                    table.ForeignKey(
                        name: "FK_TaskStatuses_Tasks_TaskRecordId",
                        column: x => x.TaskRecordId,
                        principalTable: "Tasks",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                    table.ForeignKey(
                        name: "FK_TaskStatuses_Users_UserRecordId",
                        column: x => x.UserRecordId,
                        principalTable: "Users",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "Recommendations",
                columns: table => new
                {
                    Id = table.Column<int>(type: "integer", nullable: false)
                        .Annotation("Npgsql:ValueGenerationStrategy", NpgsqlValueGenerationStrategy.IdentityByDefaultColumn),
                    Text = table.Column<string>(type: "text", nullable: false),
                    FileKeys = table.Column<string[]>(type: "text[]", nullable: false),
                    TaskStatusRecordId = table.Column<int>(type: "integer", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Recommendations", x => x.Id);
                    table.ForeignKey(
                        name: "FK_Recommendations_TaskStatuses_TaskStatusRecordId",
                        column: x => x.TaskStatusRecordId,
                        principalTable: "TaskStatuses",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateIndex(
                name: "IX_Recommendations_TaskStatusRecordId",
                table: "Recommendations",
                column: "TaskStatusRecordId");

            migrationBuilder.CreateIndex(
                name: "IX_TaskStatuses_TaskRecordId",
                table: "TaskStatuses",
                column: "TaskRecordId");

            migrationBuilder.CreateIndex(
                name: "IX_TaskStatuses_UserRecordId",
                table: "TaskStatuses",
                column: "UserRecordId");
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropTable(
                name: "Recommendations");

            migrationBuilder.DropTable(
                name: "TaskStatuses");
        }
    }
}
