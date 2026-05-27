using System.Collections.Concurrent;
using System.Collections.Generic;

namespace Mochi.Runtime;

/// <summary>
/// Broadcast stream: each call to Subscribe() gets its own unbounded queue.
/// Emit() pushes the value into every subscriber queue.
/// </summary>
public sealed class MochiStream<T>
{
    private readonly List<BlockingCollection<T>> _subs = new();

    public BlockingCollection<T> Subscribe()
    {
        var q = new BlockingCollection<T>();
        _subs.Add(q);
        return q;
    }

    public void Emit(T val)
    {
        foreach (var q in _subs) q.Add(val);
    }
}
