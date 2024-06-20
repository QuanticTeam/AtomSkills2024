
namespace backend.Core.JsonModels;

public class JsonDefect
{
    public Area Area { get; set; }
    public string[] Features { get; set; }

    public void Deconstruct(out (int x1, int y1, int x2, int y2) Item1, out string[] Item2)
    {
        Item1 = (Area.X1, Area.Y1, Area.X2, Area.Y2);
        Item2 = Features;
    }
}
