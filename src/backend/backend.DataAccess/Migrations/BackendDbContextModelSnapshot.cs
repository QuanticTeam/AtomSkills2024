﻿// <auto-generated />
using System;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Infrastructure;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;
using Npgsql.EntityFrameworkCore.PostgreSQL.Metadata;
using backend.DataAccess;

#nullable disable

namespace backend.DataAccess.Migrations
{
    [DbContext(typeof(BackendDbContext))]
    partial class BackendDbContextModelSnapshot : ModelSnapshot
    {
        protected override void BuildModel(ModelBuilder modelBuilder)
        {
#pragma warning disable 612, 618
            modelBuilder
                .HasAnnotation("ProductVersion", "8.0.5")
                .HasAnnotation("Relational:MaxIdentifierLength", 63);

            NpgsqlModelBuilderExtensions.UseIdentityByDefaultColumns(modelBuilder);

            modelBuilder.Entity("LessonRecordTaskRecord", b =>
                {
                    b.Property<int>("LessonRecordsId")
                        .HasColumnType("integer");

                    b.Property<int>("TaskRecordsId")
                        .HasColumnType("integer");

                    b.HasKey("LessonRecordsId", "TaskRecordsId");

                    b.HasIndex("TaskRecordsId");

                    b.ToTable("LessonRecordTaskRecord");
                });

            modelBuilder.Entity("LessonRecordTopicRecord", b =>
                {
                    b.Property<int>("LessonRecordsId")
                        .HasColumnType("integer");

                    b.Property<int>("TopicRecordsId")
                        .HasColumnType("integer");

                    b.HasKey("LessonRecordsId", "TopicRecordsId");

                    b.HasIndex("TopicRecordsId");

                    b.ToTable("LessonRecordTopicRecord");
                });

            modelBuilder.Entity("LessonRecordTraitRecord", b =>
                {
                    b.Property<int>("LessonRecordsId")
                        .HasColumnType("integer");

                    b.Property<int>("TraitRecordsId")
                        .HasColumnType("integer");

                    b.HasKey("LessonRecordsId", "TraitRecordsId");

                    b.HasIndex("TraitRecordsId");

                    b.ToTable("LessonRecordTraitRecord");
                });

            modelBuilder.Entity("TopicRecordTraitRecord", b =>
                {
                    b.Property<int>("TopicRecordsId")
                        .HasColumnType("integer");

                    b.Property<int>("TraitRecordsId")
                        .HasColumnType("integer");

                    b.HasKey("TopicRecordsId", "TraitRecordsId");

                    b.HasIndex("TraitRecordsId");

                    b.ToTable("TopicRecordTraitRecord");
                });

            modelBuilder.Entity("backend.DataAccess.Entities.LessonRecord", b =>
                {
                    b.Property<int>("Id")
                        .ValueGeneratedOnAdd()
                        .HasColumnType("integer");

                    NpgsqlPropertyBuilderExtensions.UseIdentityByDefaultColumn(b.Property<int>("Id"));

                    b.Property<string>("Author")
                        .IsRequired()
                        .HasColumnType("text");

                    b.Property<string>("Code")
                        .IsRequired()
                        .HasColumnType("text");

                    b.Property<string>("Content")
                        .IsRequired()
                        .HasColumnType("text");

                    b.Property<string[]>("SupplementKeys")
                        .IsRequired()
                        .HasColumnType("text[]");

                    b.Property<string>("Title")
                        .IsRequired()
                        .HasColumnType("text");

                    b.HasKey("Id");

                    b.ToTable("Lessons");
                });

            modelBuilder.Entity("backend.DataAccess.Entities.RecommendationRecord", b =>
                {
                    b.Property<int>("Id")
                        .ValueGeneratedOnAdd()
                        .HasColumnType("integer");

                    NpgsqlPropertyBuilderExtensions.UseIdentityByDefaultColumn(b.Property<int>("Id"));

                    b.Property<string[]>("FileKeys")
                        .IsRequired()
                        .HasColumnType("text[]");

                    b.Property<int>("TaskStatusRecordId")
                        .HasColumnType("integer");

                    b.Property<string>("Text")
                        .IsRequired()
                        .HasColumnType("text");

                    b.HasKey("Id");

                    b.HasIndex("TaskStatusRecordId");

                    b.ToTable("Recommendations");
                });

            modelBuilder.Entity("backend.DataAccess.Entities.SomethingRecord", b =>
                {
                    b.Property<int>("Id")
                        .ValueGeneratedOnAdd()
                        .HasColumnType("integer");

                    NpgsqlPropertyBuilderExtensions.UseIdentityByDefaultColumn(b.Property<int>("Id"));

                    b.Property<DateTime>("DateTime")
                        .HasColumnType("timestamp with time zone");

                    b.Property<string[]>("FileKeys")
                        .IsRequired()
                        .HasColumnType("text[]");

                    b.Property<int>("Integer")
                        .HasColumnType("integer");

                    b.Property<Guid>("Key")
                        .HasColumnType("uuid");

                    b.Property<string>("Name")
                        .IsRequired()
                        .HasColumnType("text");

                    b.Property<double>("Number")
                        .HasColumnType("double precision");

                    b.HasKey("Id");

                    b.ToTable("Somethings");
                });

            modelBuilder.Entity("backend.DataAccess.Entities.TaskRecord", b =>
                {
                    b.Property<int>("Id")
                        .ValueGeneratedOnAdd()
                        .HasColumnType("integer");

                    NpgsqlPropertyBuilderExtensions.UseIdentityByDefaultColumn(b.Property<int>("Id"));

                    b.Property<string>("Code")
                        .IsRequired()
                        .HasColumnType("text");

                    b.Property<string>("Content")
                        .IsRequired()
                        .HasColumnType("text");

                    b.Property<double>("Difficult")
                        .HasColumnType("double precision");

                    b.Property<string[]>("SupplementKeys")
                        .IsRequired()
                        .HasColumnType("text[]");

                    b.Property<int>("Time")
                        .HasColumnType("integer");

                    b.Property<string>("Title")
                        .IsRequired()
                        .HasColumnType("text");

                    b.HasKey("Id");

                    b.ToTable("Tasks");
                });

            modelBuilder.Entity("backend.DataAccess.Entities.TaskStatusRecord", b =>
                {
                    b.Property<int>("Id")
                        .ValueGeneratedOnAdd()
                        .HasColumnType("integer");

                    NpgsqlPropertyBuilderExtensions.UseIdentityByDefaultColumn(b.Property<int>("Id"));

                    b.Property<string>("AutomationSystemStatus")
                        .IsRequired()
                        .HasColumnType("text");

                    b.Property<DateTime>("FinishedAt")
                        .HasColumnType("timestamp with time zone");

                    b.Property<string[]>("FotoKeys")
                        .IsRequired()
                        .HasColumnType("text[]");

                    b.Property<int?>("Mark")
                        .HasColumnType("integer");

                    b.Property<DateTime>("StartedAt")
                        .HasColumnType("timestamp with time zone");

                    b.Property<string>("Status")
                        .IsRequired()
                        .HasColumnType("text");

                    b.Property<int>("TaskRecordId")
                        .HasColumnType("integer");

                    b.Property<int>("UserRecordId")
                        .HasColumnType("integer");

                    b.HasKey("Id");

                    b.HasIndex("TaskRecordId");

                    b.HasIndex("UserRecordId");

                    b.ToTable("TaskStatuses");
                });

            modelBuilder.Entity("backend.DataAccess.Entities.TopicRecord", b =>
                {
                    b.Property<int>("Id")
                        .ValueGeneratedOnAdd()
                        .HasColumnType("integer");

                    NpgsqlPropertyBuilderExtensions.UseIdentityByDefaultColumn(b.Property<int>("Id"));

                    b.Property<string>("Code")
                        .IsRequired()
                        .HasColumnType("text");

                    b.Property<string>("Description")
                        .IsRequired()
                        .HasColumnType("text");

                    b.Property<string>("Title")
                        .IsRequired()
                        .HasColumnType("text");

                    b.HasKey("Id");

                    b.ToTable("Topics");
                });

            modelBuilder.Entity("backend.DataAccess.Entities.TraitRecord", b =>
                {
                    b.Property<int>("Id")
                        .ValueGeneratedOnAdd()
                        .HasColumnType("integer");

                    NpgsqlPropertyBuilderExtensions.UseIdentityByDefaultColumn(b.Property<int>("Id"));

                    b.Property<string>("Code")
                        .IsRequired()
                        .HasColumnType("text");

                    b.Property<string>("Description")
                        .IsRequired()
                        .HasColumnType("text");

                    b.Property<string>("Name")
                        .IsRequired()
                        .HasColumnType("text");

                    b.HasKey("Id");

                    b.ToTable("Traits");
                });

            modelBuilder.Entity("backend.DataAccess.Entities.UserRecord", b =>
                {
                    b.Property<int>("Id")
                        .ValueGeneratedOnAdd()
                        .HasColumnType("integer");

                    NpgsqlPropertyBuilderExtensions.UseIdentityByDefaultColumn(b.Property<int>("Id"));

                    b.Property<string>("Email")
                        .HasColumnType("text");

                    b.Property<string>("FirstName")
                        .HasColumnType("text");

                    b.Property<Guid>("Key")
                        .HasColumnType("uuid");

                    b.Property<string>("LastName")
                        .HasColumnType("text");

                    b.Property<string>("Login")
                        .IsRequired()
                        .HasColumnType("text");

                    b.Property<string>("MiddleName")
                        .HasColumnType("text");

                    b.Property<string>("Password")
                        .IsRequired()
                        .HasColumnType("text");

                    b.Property<string>("Phone")
                        .HasColumnType("text");

                    b.Property<string>("Role")
                        .IsRequired()
                        .HasColumnType("text");

                    b.HasKey("Id");

                    b.ToTable("Users");
                });

            modelBuilder.Entity("LessonRecordTaskRecord", b =>
                {
                    b.HasOne("backend.DataAccess.Entities.LessonRecord", null)
                        .WithMany()
                        .HasForeignKey("LessonRecordsId")
                        .OnDelete(DeleteBehavior.Cascade)
                        .IsRequired();

                    b.HasOne("backend.DataAccess.Entities.TaskRecord", null)
                        .WithMany()
                        .HasForeignKey("TaskRecordsId")
                        .OnDelete(DeleteBehavior.Cascade)
                        .IsRequired();
                });

            modelBuilder.Entity("LessonRecordTopicRecord", b =>
                {
                    b.HasOne("backend.DataAccess.Entities.LessonRecord", null)
                        .WithMany()
                        .HasForeignKey("LessonRecordsId")
                        .OnDelete(DeleteBehavior.Cascade)
                        .IsRequired();

                    b.HasOne("backend.DataAccess.Entities.TopicRecord", null)
                        .WithMany()
                        .HasForeignKey("TopicRecordsId")
                        .OnDelete(DeleteBehavior.Cascade)
                        .IsRequired();
                });

            modelBuilder.Entity("LessonRecordTraitRecord", b =>
                {
                    b.HasOne("backend.DataAccess.Entities.LessonRecord", null)
                        .WithMany()
                        .HasForeignKey("LessonRecordsId")
                        .OnDelete(DeleteBehavior.Cascade)
                        .IsRequired();

                    b.HasOne("backend.DataAccess.Entities.TraitRecord", null)
                        .WithMany()
                        .HasForeignKey("TraitRecordsId")
                        .OnDelete(DeleteBehavior.Cascade)
                        .IsRequired();
                });

            modelBuilder.Entity("TopicRecordTraitRecord", b =>
                {
                    b.HasOne("backend.DataAccess.Entities.TopicRecord", null)
                        .WithMany()
                        .HasForeignKey("TopicRecordsId")
                        .OnDelete(DeleteBehavior.Cascade)
                        .IsRequired();

                    b.HasOne("backend.DataAccess.Entities.TraitRecord", null)
                        .WithMany()
                        .HasForeignKey("TraitRecordsId")
                        .OnDelete(DeleteBehavior.Cascade)
                        .IsRequired();
                });

            modelBuilder.Entity("backend.DataAccess.Entities.RecommendationRecord", b =>
                {
                    b.HasOne("backend.DataAccess.Entities.TaskStatusRecord", "TaskStatusRecord")
                        .WithMany("RecommendationRecords")
                        .HasForeignKey("TaskStatusRecordId")
                        .OnDelete(DeleteBehavior.Cascade)
                        .IsRequired();

                    b.Navigation("TaskStatusRecord");
                });

            modelBuilder.Entity("backend.DataAccess.Entities.TaskStatusRecord", b =>
                {
                    b.HasOne("backend.DataAccess.Entities.TaskRecord", "TaskRecord")
                        .WithMany("TaskStatusRecords")
                        .HasForeignKey("TaskRecordId")
                        .OnDelete(DeleteBehavior.Cascade)
                        .IsRequired();

                    b.HasOne("backend.DataAccess.Entities.UserRecord", "UserRecord")
                        .WithMany("TaskStatusRecords")
                        .HasForeignKey("UserRecordId")
                        .OnDelete(DeleteBehavior.Cascade)
                        .IsRequired();

                    b.Navigation("TaskRecord");

                    b.Navigation("UserRecord");
                });

            modelBuilder.Entity("backend.DataAccess.Entities.TaskRecord", b =>
                {
                    b.Navigation("TaskStatusRecords");
                });

            modelBuilder.Entity("backend.DataAccess.Entities.TaskStatusRecord", b =>
                {
                    b.Navigation("RecommendationRecords");
                });

            modelBuilder.Entity("backend.DataAccess.Entities.UserRecord", b =>
                {
                    b.Navigation("TaskStatusRecords");
                });
#pragma warning restore 612, 618
        }
    }
}
