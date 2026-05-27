namespace Mochi.Runtime.Math;

using Mochi.Runtime.Errors;

/// <summary>Math helpers that match Mochi vm3 semantics.</summary>
public static class MochiMath
{
    /// <summary>Integer division; throws MochiDivideByZeroError when b == 0.</summary>
    public static long IntDiv(long a, long b)
    {
        if (b == 0L) throw new MochiDivideByZeroError();
        return a / b;
    }

    /// <summary>Integer modulo; throws MochiDivideByZeroError when b == 0.</summary>
    public static long IntMod(long a, long b)
    {
        if (b == 0L) throw new MochiDivideByZeroError();
        return a % b;
    }
}
