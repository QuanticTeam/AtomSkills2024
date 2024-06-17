using Microsoft.EntityFrameworkCore.Migrations;
using Npgsql.EntityFrameworkCore.PostgreSQL.Metadata;

#nullable disable

namespace backend.DataAccess.Migrations
{
    /// <inheritdoc />
    public partial class NewTables : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.CreateTable(
                name: "Lessons",
                columns: table => new
                {
                    Id = table.Column<int>(type: "integer", nullable: false)
                        .Annotation("Npgsql:ValueGenerationStrategy", NpgsqlValueGenerationStrategy.IdentityByDefaultColumn),
                    Code = table.Column<string>(type: "text", nullable: false),
                    Title = table.Column<string>(type: "text", nullable: false),
                    Content = table.Column<string>(type: "text", nullable: false),
                    Author = table.Column<string>(type: "text", nullable: false),
                    SupplementKeys = table.Column<string[]>(type: "text[]", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Lessons", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "Tasks",
                columns: table => new
                {
                    Id = table.Column<int>(type: "integer", nullable: false)
                        .Annotation("Npgsql:ValueGenerationStrategy", NpgsqlValueGenerationStrategy.IdentityByDefaultColumn),
                    Code = table.Column<string>(type: "text", nullable: false),
                    Title = table.Column<string>(type: "text", nullable: false),
                    Content = table.Column<string>(type: "text", nullable: false),
                    Difficult = table.Column<double>(type: "double precision", nullable: false),
                    Time = table.Column<int>(type: "integer", nullable: false),
                    SupplementKeys = table.Column<string[]>(type: "text[]", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Tasks", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "Topics",
                columns: table => new
                {
                    Id = table.Column<int>(type: "integer", nullable: false)
                        .Annotation("Npgsql:ValueGenerationStrategy", NpgsqlValueGenerationStrategy.IdentityByDefaultColumn),
                    Code = table.Column<string>(type: "text", nullable: false),
                    Title = table.Column<string>(type: "text", nullable: false),
                    Description = table.Column<string>(type: "text", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Topics", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "Traits",
                columns: table => new
                {
                    Id = table.Column<int>(type: "integer", nullable: false)
                        .Annotation("Npgsql:ValueGenerationStrategy", NpgsqlValueGenerationStrategy.IdentityByDefaultColumn),
                    Code = table.Column<string>(type: "text", nullable: false),
                    Name = table.Column<string>(type: "text", nullable: false),
                    Description = table.Column<string>(type: "text", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Traits", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "LessonRecordTaskRecord",
                columns: table => new
                {
                    LessonRecordsId = table.Column<int>(type: "integer", nullable: false),
                    TaskRecordsId = table.Column<int>(type: "integer", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_LessonRecordTaskRecord", x => new { x.LessonRecordsId, x.TaskRecordsId });
                    table.ForeignKey(
                        name: "FK_LessonRecordTaskRecord_Lessons_LessonRecordsId",
                        column: x => x.LessonRecordsId,
                        principalTable: "Lessons",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                    table.ForeignKey(
                        name: "FK_LessonRecordTaskRecord_Tasks_TaskRecordsId",
                        column: x => x.TaskRecordsId,
                        principalTable: "Tasks",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "LessonRecordTopicRecord",
                columns: table => new
                {
                    LessonRecordsId = table.Column<int>(type: "integer", nullable: false),
                    TopicRecordsId = table.Column<int>(type: "integer", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_LessonRecordTopicRecord", x => new { x.LessonRecordsId, x.TopicRecordsId });
                    table.ForeignKey(
                        name: "FK_LessonRecordTopicRecord_Lessons_LessonRecordsId",
                        column: x => x.LessonRecordsId,
                        principalTable: "Lessons",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                    table.ForeignKey(
                        name: "FK_LessonRecordTopicRecord_Topics_TopicRecordsId",
                        column: x => x.TopicRecordsId,
                        principalTable: "Topics",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "LessonRecordTraitRecord",
                columns: table => new
                {
                    LessonRecordsId = table.Column<int>(type: "integer", nullable: false),
                    TraitRecordsId = table.Column<int>(type: "integer", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_LessonRecordTraitRecord", x => new { x.LessonRecordsId, x.TraitRecordsId });
                    table.ForeignKey(
                        name: "FK_LessonRecordTraitRecord_Lessons_LessonRecordsId",
                        column: x => x.LessonRecordsId,
                        principalTable: "Lessons",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                    table.ForeignKey(
                        name: "FK_LessonRecordTraitRecord_Traits_TraitRecordsId",
                        column: x => x.TraitRecordsId,
                        principalTable: "Traits",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "TopicRecordTraitRecord",
                columns: table => new
                {
                    TopicRecordsId = table.Column<int>(type: "integer", nullable: false),
                    TraitRecordsId = table.Column<int>(type: "integer", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_TopicRecordTraitRecord", x => new { x.TopicRecordsId, x.TraitRecordsId });
                    table.ForeignKey(
                        name: "FK_TopicRecordTraitRecord_Topics_TopicRecordsId",
                        column: x => x.TopicRecordsId,
                        principalTable: "Topics",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                    table.ForeignKey(
                        name: "FK_TopicRecordTraitRecord_Traits_TraitRecordsId",
                        column: x => x.TraitRecordsId,
                        principalTable: "Traits",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateIndex(
                name: "IX_LessonRecordTaskRecord_TaskRecordsId",
                table: "LessonRecordTaskRecord",
                column: "TaskRecordsId");

            migrationBuilder.CreateIndex(
                name: "IX_LessonRecordTopicRecord_TopicRecordsId",
                table: "LessonRecordTopicRecord",
                column: "TopicRecordsId");

            migrationBuilder.CreateIndex(
                name: "IX_LessonRecordTraitRecord_TraitRecordsId",
                table: "LessonRecordTraitRecord",
                column: "TraitRecordsId");

            migrationBuilder.CreateIndex(
                name: "IX_TopicRecordTraitRecord_TraitRecordsId",
                table: "TopicRecordTraitRecord",
                column: "TraitRecordsId");
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropTable(
                name: "LessonRecordTaskRecord");

            migrationBuilder.DropTable(
                name: "LessonRecordTopicRecord");

            migrationBuilder.DropTable(
                name: "LessonRecordTraitRecord");

            migrationBuilder.DropTable(
                name: "TopicRecordTraitRecord");

            migrationBuilder.DropTable(
                name: "Tasks");

            migrationBuilder.DropTable(
                name: "Lessons");

            migrationBuilder.DropTable(
                name: "Topics");

            migrationBuilder.DropTable(
                name: "Traits");
        }
    }
}
