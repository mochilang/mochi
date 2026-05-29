<?php

declare(strict_types=1);

namespace Mochi\Runtime;

/**
 * IO holds the print primitives used by lowered Mochi programs. Phase 0
 * ships only the surface area required to compile the skeleton; Phase 1
 * adds full coverage of the scalar print family. The runtime never emits
 * a trailing newline by itself, matching vm3 semantics.
 */
final class IO
{
    /**
     * Print a string value to stdout.
     */
    public static function printString(string $value): void
    {
        echo $value;
    }

    /**
     * Print an integer value to stdout. PHP's int can be 32-bit on some
     * platforms; the lowerer routes wide-int prints through printBigInt
     * once Phase 2 wires GMP. Phase 0 only uses the native form.
     */
    public static function printInt(int $value): void
    {
        echo $value;
    }

    /**
     * Print a boolean value as the lowercase literal "true" or "false"
     * (matching the Mochi reference vm3 output, not PHP's empty-string
     * convention for false).
     */
    public static function printBool(bool $value): void
    {
        echo $value ? 'true' : 'false';
    }

    /**
     * Print a 64-bit float value. The formatter mirrors Go's
     * strconv.FormatFloat 'g' -1 64 contract so emitted Mochi programs
     * round-trip byte-equal across hosts.
     */
    public static function printFloat(float $value): void
    {
        if (is_nan($value)) {
            echo 'NaN';
            return;
        }
        if (is_infinite($value)) {
            echo $value < 0 ? '-Inf' : '+Inf';
            return;
        }
        if ((float) (int) $value === $value && abs($value) < 1.0e15) {
            echo $value < 0 ? '-' . number_format(abs($value), 0, '.', '') : number_format($value, 0, '.', '');
            return;
        }
        echo (string) $value;
    }

    /**
     * Print a literal newline. The lowerer emits this between consecutive
     * print calls when the source had no explicit separator, matching the
     * vm3 println behaviour.
     */
    public static function newline(): void
    {
        echo "\n";
    }
}
