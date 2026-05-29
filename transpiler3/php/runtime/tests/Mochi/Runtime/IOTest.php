<?php

declare(strict_types=1);

namespace Mochi\Runtime\Tests;

use Mochi\Runtime\IO;
use PHPUnit\Framework\TestCase;

/**
 * Phase 0 smoke tests for the IO print family. The lowerer routes every
 * print call through one of these helpers, so this test fails fast when
 * a future change accidentally swallows the value or adds an unexpected
 * newline.
 */
final class IOTest extends TestCase
{
    public function testPrintStringWritesValue(): void
    {
        $this->expectOutputString('hello');
        IO::printString('hello');
    }

    public function testPrintIntWritesValue(): void
    {
        $this->expectOutputString('42');
        IO::printInt(42);
    }

    public function testPrintBoolWritesTrueOrFalse(): void
    {
        $this->expectOutputString('true');
        IO::printBool(true);
    }

    public function testPrintBoolFalseWritesFalse(): void
    {
        $this->expectOutputString('false');
        IO::printBool(false);
    }

    public function testPrintFloatFormatsAsG(): void
    {
        $this->expectOutputString('3.14');
        IO::printFloat(3.14);
    }

    public function testPrintFloatNaN(): void
    {
        $this->expectOutputString('NaN');
        IO::printFloat(NAN);
    }

    public function testPrintFloatPositiveInfinity(): void
    {
        $this->expectOutputString('+Inf');
        IO::printFloat(INF);
    }

    public function testPrintFloatNegativeInfinity(): void
    {
        $this->expectOutputString('-Inf');
        IO::printFloat(-INF);
    }

    public function testNewlineEmitsSingleLineFeed(): void
    {
        $this->expectOutputString("\n");
        IO::newline();
    }
}
