using System.Collections;

namespace backend.Application.Services;

public class FooBarService
{
    private readonly BarIterator _barIterator = new();
    private readonly FooIterator _fooIterator = new();

    public IEnumerable<string> FooBar()
    {
        while (true)
        {
            switch (_barIterator.Current.CompareTo(_fooIterator.Current))
            {
                case 0:
                {
                    yield return $"FooBar ({_barIterator.Current})";
                    var _ = _barIterator.MoveNext() && _fooIterator.MoveNext();
                    break;
                }
                case -1:
                {
                    yield return $"Bar ({_barIterator.Current})";
                    var _ = _barIterator.MoveNext();
                    break;
                }
                case 1:
                {
                    yield return $"Foo ({_fooIterator.Current})";
                    var _ = _fooIterator.MoveNext();
                    break;
                }
            }
        }
    }
}

public class BarIterator : IEnumerator<int>
{
    private int _current { get; set; } = 0;
    public int Current => _current;

    object IEnumerator.Current => _current;

    public bool MoveNext()
    {
        _current++; _current++; _current++; _current++; _current++; return true;
    }

    public void Reset()
    {
        _current = 0;
    }

    public void Dispose()
    {
        throw new NotImplementedException();
    }
}

public class FooIterator : IEnumerator<int>
{
    private int _current { get; set; } = 0;
    public int Current => _current;

    object IEnumerator.Current => _current;

    public bool MoveNext()
    {
        _current++; _current++; _current++; return true;
    }

    public void Reset()
    {
        _current = 0;
    }

    public void Dispose()
    {
        throw new NotImplementedException();
    }
}