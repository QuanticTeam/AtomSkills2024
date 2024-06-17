using backend.Core.Models;
using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace backend.DataAccess.Migrations
{
    /// <inheritdoc />
    public partial class AddTestUsers : Migration
    {
        private static readonly string[] columns = new []{ "Key", "Login", "Password", "Role", "FirstName", "LastName", "MiddleName" };

        private static readonly object[] mentor = new object[] { Guid.NewGuid(), "mentor", BCrypt.Net.BCrypt.EnhancedHashPassword("mentor"), UserRole.Mentor.ToString(), "Вася", "Пупкин", "Василич" };
        
        private static readonly object[] student1 = new object[] { Guid.NewGuid(), "student1", BCrypt.Net.BCrypt.EnhancedHashPassword("student1"), UserRole.Student.ToString(), "Витя", "Иванов", "Димыч" };
        
        private static readonly object[] student2 = new object[] { Guid.NewGuid(), "student2", BCrypt.Net.BCrypt.EnhancedHashPassword("student2"), UserRole.Student.ToString(), "Саня", "Сидоров", "Витич" };
        
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.InsertData(
                table: "Users", 
                columns: columns,
                values: student1);
            
            migrationBuilder.InsertData(
                table: "Users", 
                columns: columns,
                values: mentor);
            
            migrationBuilder.InsertData(
                table: "Users", 
                columns: columns,
                values: student2);
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DeleteData(
                table: "Users", 
                keyColumn: columns.First(),
                keyColumnType: "Guid",
                keyValue: mentor.First());
            
            migrationBuilder.DeleteData(
                table: "Users", 
                keyColumn: columns.First(),
                keyColumnType: "Guid",
                keyValue: student1.First());
            
            migrationBuilder.DeleteData(
                table: "Users", 
                keyColumn: columns.First(),
                keyColumnType: "Guid",
                keyValue: student2.First());
        }
    }
}
