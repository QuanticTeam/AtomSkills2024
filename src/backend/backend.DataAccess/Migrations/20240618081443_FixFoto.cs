using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace backend.DataAccess.Migrations
{
    /// <inheritdoc />
    public partial class FixFoto : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropForeignKey(
                name: "FK_FotoRecord_TaskStatuses_TaskStatusRecordId",
                table: "FotoRecord");

            migrationBuilder.DropPrimaryKey(
                name: "PK_FotoRecord",
                table: "FotoRecord");

            migrationBuilder.RenameTable(
                name: "FotoRecord",
                newName: "Fotos");

            migrationBuilder.RenameIndex(
                name: "IX_FotoRecord_TaskStatusRecordId",
                table: "Fotos",
                newName: "IX_Fotos_TaskStatusRecordId");

            migrationBuilder.AddPrimaryKey(
                name: "PK_Fotos",
                table: "Fotos",
                column: "Id");

            migrationBuilder.AddForeignKey(
                name: "FK_Fotos_TaskStatuses_TaskStatusRecordId",
                table: "Fotos",
                column: "TaskStatusRecordId",
                principalTable: "TaskStatuses",
                principalColumn: "Id",
                onDelete: ReferentialAction.Cascade);
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropForeignKey(
                name: "FK_Fotos_TaskStatuses_TaskStatusRecordId",
                table: "Fotos");

            migrationBuilder.DropPrimaryKey(
                name: "PK_Fotos",
                table: "Fotos");

            migrationBuilder.RenameTable(
                name: "Fotos",
                newName: "FotoRecord");

            migrationBuilder.RenameIndex(
                name: "IX_Fotos_TaskStatusRecordId",
                table: "FotoRecord",
                newName: "IX_FotoRecord_TaskStatusRecordId");

            migrationBuilder.AddPrimaryKey(
                name: "PK_FotoRecord",
                table: "FotoRecord",
                column: "Id");

            migrationBuilder.AddForeignKey(
                name: "FK_FotoRecord_TaskStatuses_TaskStatusRecordId",
                table: "FotoRecord",
                column: "TaskStatusRecordId",
                principalTable: "TaskStatuses",
                principalColumn: "Id",
                onDelete: ReferentialAction.Cascade);
        }
    }
}
