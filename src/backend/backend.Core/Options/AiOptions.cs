namespace backend.Core.Options;

public class AiOptions
{
    public bool AS_2024_ENV_AI_OFF { get; set; } = false;

    public string AS_2024_ENV_HOST { get; set; }
    
    // Порт окружения (по-умолчанию 10240)
    public int AS_2024_ENV_PORT { get; set; } = 10240;
    
    // Директория с файлами справочных данных (по-умолчанию ./dict) 
    public string AS_2024_ENV_DICT_DIR { get; set; }
    
    // Флаг использования кэша (по-умолчанию true). Выставленном флаге в false результат функции проверки для одного и того же изображения будет разным
    public bool AS_2024_ENV_USE_CACHE { get; set; } = true;
    
    // Минимальный радиус зоны особенностей (в пикселах). Просчитывается для координат x2 и y2
    public int AS_2024_ENV_MIN_R { get; set; } 
    
    // Максиимальный радиус зоны особенностей (в пикселах) Просчитывается для координат x2 и y2.
    public int AS_2024_ENV_MAX_R { get; set; }
    
    // Максимальное количество зон особенностей
    public int AS_2024_ENV_MAX_FEATURES { get; set; } 
    
    // Максимальное количество кодов особенностей в найденых зонах
    public int AS_2024_ENV_MAX_CODES { get; set; }
}