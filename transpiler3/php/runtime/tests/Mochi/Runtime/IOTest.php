<?php

declare(strict_types=1);

namespace Mochi\Runtime\Tests;

use Mochi\Runtime\IO;
use PHPUnit\Framework\TestCase;

/**
 * Phase 0/1 smoke tests for the IO print family. The lowerer routes every
 * print call through one of these helpers, so this test fails fast when
 * a future change accidentally swallows the value or drops the trailing
 * newline.
 */
final class IOTest extends TestCase
{
    public function testPrintStringWritesValueWithNewline(): void
    {
        $this->expectOutputString("hello\n");
        IO::printString('hello');
    }

    public function testPrintIntWritesValueWithNewline(): void
    {
        $this->expectOutputString("42\n");
        IO::printInt(42);
    }

    public function testPrintBoolTrueWritesTrueWithNewline(): void
    {
        $this->expectOutputString("true\n");
        IO::printBool(true);
    }

    public function testPrintBoolFalseWritesFalseWithNewline(): void
    {
        $this->expectOutputString("false\n");
        IO::printBool(false);
    }

    public function testPrintFloatNonIntegerKeepsDecimal(): void
    {
        $this->expectOutputString("3.14\n");
        IO::printFloat(3.14);
    }

    public function testPrintFloatWholeNumberDropsDecimal(): void
    {
        $this->expectOutputString("4\n");
        IO::printFloat(4.0);
    }

    public function testPrintFloatNaN(): void
    {
        $this->expectOutputString("NaN\n");
        IO::printFloat(NAN);
    }

    public function testPrintFloatPositiveInfinity(): void
    {
        $this->expectOutputString("+Inf\n");
        IO::printFloat(INF);
    }

    public function testPrintFloatNegativeInfinity(): void
    {
        $this->expectOutputString("-Inf\n");
        IO::printFloat(-INF);
    }
}
