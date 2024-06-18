using backend.Core.Abstractions;

namespace backend.Application.Services;

public class DefectDictionaryService : IDefectDictionaryService
{
    public string GetDefectText(string code)
    {
        return _mapping[code];
    }

    private readonly Dictionary<string, string> _mapping = new Dictionary<string, string>()
    {
        { "AUS", "Асимметрия углового шва" },
        { "MSP", "Брызги металла" },
        { "TINC", "Вольфрамовое включение" },
        { "INC", "Включение" },
        {"SINC", "Включение одиночное"},
        {"CRS", "Вогнутость корня шва"},
        {"EPRS", "Выпуклость (превышение проплавления) корня шва"},
        {"DRL", "Глубокий валик"},
        {"CCR", "Кратерная трещина. Трещина в кратере"},
        {"SCC", "Кратер. Усадочная раковина сварного шва"},
        {"LPOR", "Линия пор. Линейная пористость"},
        {"MIW", "Максимальная ширина включения"},
        {"MSW", "Максимальный размер включения"},
        {"LEP", "Местное превышение проплава"},
        {"NMB", "Неплавящийся наплыв"},
        {"BD", "Наплыв"},
        {"IWP", "Неправильный профиль сварного шва"},
        {"LOF", "Непровар. Неполный провар"},
        {"DCN", "Несплошность"},
        {"OIN", "Окисное включение"},
        {"DTC", "Отслоение"},
        {"PSR", "Плохое возобновление шва"},
        {"UCT", "Подрез"},
        {"POR", "Поры"},
        {"EOC", "Превышение выпуклости"},
        {"EWR", "Превышение усиления сварного шва"},
        {"DTL", "Прерывистая линия"},
        {"LCWJ", "Продольная трещина сварного соединения"},
        {"PWS", "Прохождение сварного шва"},
        {"RDC", "Радиационная трещина"},
        {"BCWJ", "Разветвленная трещина сварного соединения"},
        {"COI", "Скопление включений"},
        {"FIW", "Свищ в сварном шве"},
        {"TSC", "Трещина поперечная"},
        {"CWJ", "Трещина сварного соединения. Трещина"},
        {"RBWB", "Углубление (западание) между валиками шва"},
        {"SCAV", "Усадочные раковины"},
        {"FLIN", "Флюсовое включение"},
        {"SIIW", "Шлаковое включение сварного шва"},
        {"USS", "Неровная поверхность шва"},
        {"USW", "Неровная ширина шва"}
    };
}