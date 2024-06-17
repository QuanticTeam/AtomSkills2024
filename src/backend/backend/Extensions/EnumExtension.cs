using backend.Contracts;

namespace backend.Extensions;

public static class EnumExtension
{
    public static List<EnumResponse> GetEnumValues<TEnum>() where TEnum : Enum
    {
        return Enum.GetValues(typeof(TEnum))
            .Cast<TEnum>()
            .Select(e => new EnumResponse(Convert.ToInt32(e), e.ToString()))
            .ToList();
    }
}