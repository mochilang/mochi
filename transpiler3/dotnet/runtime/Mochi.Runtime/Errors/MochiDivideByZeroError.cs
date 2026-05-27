namespace Mochi.Runtime.Errors;

/// <summary>Raised by MochiMath.IntDiv when the divisor is zero. Matches vm3 MOCHI_ERR_DIVZERO.</summary>
public sealed class MochiDivideByZeroError : Exception
{
    public MochiDivideByZeroError() : base("integer divide by zero") { }
}
