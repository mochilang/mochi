<?php

declare(strict_types=1);

namespace Mochi\Runtime;

/**
 * IO holds the print primitives used by lowered Mochi programs. Every
 * helper appends a trailing newline, matching the Mochi reference vm3
 * \\println behaviour. The lowerer routes calls through these helpers
 * so the runtime owns formatting (e.g. \\NaN / \\+Inf / \\-Inf and the
 * Go-style \\strconv.FormatFloat 'g' -1 64 round-trip).
 *
 * @api  Public runtime entry point. Lowered Mochi programs live
 *       outside `src/`, so Psalm's UnusedClass check would otherwise
 *       flag this. The @api tag tells Psalm this is intentionally an
 *       externally-consumed surface.
 */
final class IO
{
    /**
     * Print a string followed by a newline.
     */
    public static function printString(string $value): void
    {
        echo $value, "\n";
    }

    /**
     * Print a signed 64-bit integer followed by a newline. PHP's int is
     * platform-width; the lowerer reroutes wide-int prints through
     * printBigInt once Phase 2 wires GMP. Phase 1 only uses the native
     * form.
     */
    public static function printInt(int $value): void
    {
        echo $value, "\n";
    }

    /**
     * Print a boolean as the lowercase literal "true" or "false"
     * (matching the Mochi reference vm3 output, not PHP's empty-string
     * convention for false), followed by a newline.
     */
    public static function printBool(bool $value): void
    {
        echo $value ? "true\n" : "false\n";
    }

    /**
     * Print a 64-bit float followed by a newline. The formatter mirrors
     * Go's strconv.FormatFloat 'g' -1 64 contract so emitted Mochi
     * programs round-trip byte-equal across hosts.
     */
    public static function printFloat(float $value): void
    {
        if (is_nan($value)) {
            echo "NaN\n";
            return;
        }
        if (is_infinite($value)) {
            echo $value < 0 ? "-Inf\n" : "+Inf\n";
            return;
        }
        // Whole-number values print without a decimal point (4.0 → "4"),
        // matching Go's %g behaviour for IEEE 754 binary64 integers.
        if ((float) (int) $value === $value && abs($value) < 1.0e15) {
            $i = (int) $value;
            echo $i, "\n";
            return;
        }
        echo $value, "\n";
    }
}
