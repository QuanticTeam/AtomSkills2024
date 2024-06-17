using System.Reflection;
using System.Text.Json;
using backend.Core.Abstractions;
using backend.Core.JsonModels;
using backend.Core.Options;
using Microsoft.Extensions.Options;
using Microsoft.IdentityModel.Protocols.Configuration;
using Microsoft.IdentityModel.Tokens;
using JsonTrait = backend.Core.JsonModels.JsonTrait;

namespace backend.Application.Services;

public class ContentLoadService : IContentLoadService
{
    private readonly ContentLoadOptions _options;
    private readonly string _lessonFolder;

    private readonly JsonSerializerOptions _serializeOptions;

    public ContentLoadService(IOptions<ContentLoadOptions> options)
    {
        _options = options.Value;

        if (_options.FolderPath.IsNullOrEmpty())
        {
            throw new InvalidConfigurationException($"Опция '{nameof(_options.FolderPath)}' должна быть добавлена в конфиг");
        }

        var currentFolder = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)!;
        _lessonFolder = Path.Combine(currentFolder, _options.FolderPath);

        if (!Directory.Exists(_lessonFolder))
        {
            throw new InvalidConfigurationException($"Папка '{_options.FolderPath}' не найдена");
        }

        if (_options.TraitFileTemplate.IsNullOrEmpty())
        {
            throw new InvalidConfigurationException($"Опция '{nameof(_options.TraitFileTemplate)}' должна быть добавлена в конфиг");
        }

        if (_options.TopicFileTemplate.IsNullOrEmpty())
        {
            throw new InvalidConfigurationException($"Опция '{nameof(_options.TopicFileTemplate)}' должна быть добавлена в конфиг");
        }

        if (_options.LessonFileTemplate.IsNullOrEmpty())
        {
            throw new InvalidConfigurationException($"Опция '{nameof(_options.LessonFileTemplate)}' должна быть добавлена в конфиг");
        }

        if (_options.TaskFileTemplate.IsNullOrEmpty())
        {
            throw new InvalidConfigurationException($"Опция '{nameof(_options.TaskFileTemplate)}' должна быть добавлена в конфиг");
        }

        _serializeOptions = new JsonSerializerOptions
        {
            PropertyNameCaseInsensitive = true
        };
    }

    public IEnumerable<JsonTrait> LoadTraits()
    {
        return LoadLessonFolder<JsonTrait>(_options.TraitFileTemplate).Select(s => s.Item1);
    }

    public IEnumerable<JsonTopic> LoadTopics()
    {
        return LoadLessonFolder<JsonTopic>(_options.TopicFileTemplate).Select(s => s.Item1);
    }

    public IEnumerable<JsonLesson> LoadLessons()
    {
        foreach (var (lesson, directory) in LoadLessonFolder<JsonLesson>(_options.LessonFileTemplate))
        {
            lesson.Supplement = AdjustFilePath(lesson.Supplement, directory);
            yield return lesson;
        }
    }

    public IEnumerable<JsonTask> LoadTasks()
    {
        foreach (var (task, directory) in LoadLessonFolder<JsonTask>(_options.TaskFileTemplate))
        {
            task.Supplement = AdjustFilePath(task.Supplement, directory);
            yield return task;
        }
    }

    private List<JsonSupplement> AdjustFilePath(List<JsonSupplement> supplements, DirectoryInfo directory)
    {
        // Для относительного пути:
        // var relativePath = Path.GetRelativePath(_lessonFolder, directory.FullName);

        return supplements.Select(s => new JsonSupplement
            {
                File = Path.Combine(directory.FullName, s.File),
                Title = s.Title,
            }).ToList();
    }
    
    private IEnumerable<(T, DirectoryInfo)> LoadLessonFolder<T>(string searchPattern)
    {
        return LoadRecursive<T>(searchPattern, new DirectoryInfo(_lessonFolder));
    }

    private IEnumerable<(T, DirectoryInfo)> LoadRecursive<T>(string searchPattern, DirectoryInfo directory)
    {
        foreach (var subfolder in directory.GetDirectories().OfType<DirectoryInfo>())
        {
            foreach (var entity in LoadRecursive<T>(searchPattern, subfolder))
            {
                yield return entity;
            }
        }

        foreach (var file in directory.GetFiles(searchPattern).OfType<FileInfo>())
        {
            foreach (var entity in LoadFromFile<T>(file))
            {
                yield return (entity, directory);
            };
        }
    }

    private List<T> LoadFromFile<T>(FileInfo fileInfo)
    {
        try
        {
            var json = File.ReadAllText(fileInfo.FullName);
            var trait = JsonSerializer.Deserialize<T>(json, _serializeOptions);

            if (trait != null)
            {
                return new List<T> { trait! };
            }
        } catch {}

        try
        {
            var json = File.ReadAllText(fileInfo.FullName);
            var traits = JsonSerializer.Deserialize<List<T>>(json, _serializeOptions);

            if (traits != null)
            {
                return traits;
            }
        } catch {}

        Console.Out.WriteLine($"WARN: Can't read file {fileInfo.Name}");
        return Enumerable.Empty<T>().ToList();
    }
}