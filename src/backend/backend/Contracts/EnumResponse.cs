namespace backend.Contracts;

public record EnumResponse(int id, string role)
{
    public int Id = id;

    public string Name = role;
}