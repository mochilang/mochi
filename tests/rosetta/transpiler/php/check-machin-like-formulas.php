<?php
ini_set('memory_limit', '-1');
function _gcd($a, $b) {
    if (function_exists('bcadd')) {
        if (bccomp($a, '0') < 0) $a = bcsub('0', $a);
        if (bccomp($b, '0') < 0) $b = bcsub('0', $b);
        while (bccomp($b, '0') != 0) {
            $t = bcmod($a, $b);
            $a = $b;
            $b = $t;
        }
        return $a;
    }
    $a = abs($a);
    $b = abs($b);
    while ($b != 0) {
        $t = $a % $b;
        $a = $b;
        $b = $t;
    }
    return $a;
}
function _bigrat($n, $d = 1) {
    if (is_array($n) && isset($n['num']) && isset($n['den']) && $d === null) {
        return $n;
    }
    if ($d === null) { $d = 1; }
    if (function_exists('bcadd')) {
        $n = (string)$n; $d = (string)$d;
        if (bccomp($d, '0') < 0) { $n = bcsub('0', $n); $d = bcsub('0', $d); }
        $g = _gcd($n, $d);
        return ['num' => bcdiv($n, $g, 0), 'den' => bcdiv($d, $g, 0)];
    }
    if ($d < 0) { $n = -$n; $d = -$d; }
    $g = _gcd($n, $d);
    return ['num' => intdiv($n, $g), 'den' => intdiv($d, $g)];
}
function _add($a, $b) {
    if (function_exists('bcadd')) {
        $n = bcadd(bcmul(num($a), denom($b)), bcmul(num($b), denom($a)));
        $d = bcmul(denom($a), denom($b));
        return _bigrat($n, $d);
    }
    return _bigrat(num($a) * denom($b) + num($b) * denom($a), denom($a) * denom($b));
}
function _sub($a, $b) {
    if (function_exists('bcadd')) {
        $n = bcsub(bcmul(num($a), denom($b)), bcmul(num($b), denom($a)));
        $d = bcmul(denom($a), denom($b));
        return _bigrat($n, $d);
    }
    return _bigrat(num($a) * denom($b) - num($b) * denom($a), denom($a) * denom($b));
}
function _mul($a, $b) {
    if (function_exists('bcadd')) {
        $n = bcmul(num($a), num($b));
        $d = bcmul(denom($a), denom($b));
        return _bigrat($n, $d);
    }
    return _bigrat(num($a) * num($b), denom($a) * denom($b));
}
function _div($a, $b) {
    if (function_exists('bcadd')) {
        $n = bcmul(num($a), denom($b));
        $d = bcmul(denom($a), num($b));
        return _bigrat($n, $d);
    }
    return _bigrat(num($a) * denom($b), denom($a) * num($b));
}
function num($x) {
    if (is_array($x) && array_key_exists('num', $x)) return $x['num'];
    return $x;
}
function denom($x) {
    if (is_array($x) && array_key_exists('den', $x)) return $x['den'];
    return function_exists('bcadd') ? '1' : 1;
}
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
function br($n, $d) {
  global $testCases, $format, $tanEval, $tans;
  return _div((_bigrat($n, null)), _bigrat((_bigrat($d, null)), null));
}
$testCases = [[['a' => 1, 'n' => 1, 'd' => 2], ['a' => 1, 'n' => 1, 'd' => 3]], [['a' => 2, 'n' => 1, 'd' => 3], ['a' => 1, 'n' => 1, 'd' => 7]], [['a' => 4, 'n' => 1, 'd' => 5], ['a' => -1, 'n' => 1, 'd' => 239]], [['a' => 5, 'n' => 1, 'd' => 7], ['a' => 2, 'n' => 3, 'd' => 79]], [['a' => 1, 'n' => 1, 'd' => 2], ['a' => 1, 'n' => 1, 'd' => 5], ['a' => 1, 'n' => 1, 'd' => 8]], [['a' => 4, 'n' => 1, 'd' => 5], ['a' => -1, 'n' => 1, 'd' => 70], ['a' => 1, 'n' => 1, 'd' => 99]], [['a' => 5, 'n' => 1, 'd' => 7], ['a' => 4, 'n' => 1, 'd' => 53], ['a' => 2, 'n' => 1, 'd' => 4443]], [['a' => 6, 'n' => 1, 'd' => 8], ['a' => 2, 'n' => 1, 'd' => 57], ['a' => 1, 'n' => 1, 'd' => 239]], [['a' => 8, 'n' => 1, 'd' => 10], ['a' => -1, 'n' => 1, 'd' => 239], ['a' => -4, 'n' => 1, 'd' => 515]], [['a' => 12, 'n' => 1, 'd' => 18], ['a' => 8, 'n' => 1, 'd' => 57], ['a' => -5, 'n' => 1, 'd' => 239]], [['a' => 16, 'n' => 1, 'd' => 21], ['a' => 3, 'n' => 1, 'd' => 239], ['a' => 4, 'n' => 3, 'd' => 1042]], [['a' => 22, 'n' => 1, 'd' => 28], ['a' => 2, 'n' => 1, 'd' => 443], ['a' => -5, 'n' => 1, 'd' => 1393], ['a' => -10, 'n' => 1, 'd' => 11018]], [['a' => 22, 'n' => 1, 'd' => 38], ['a' => 17, 'n' => 7, 'd' => 601], ['a' => 10, 'n' => 7, 'd' => 8149]], [['a' => 44, 'n' => 1, 'd' => 57], ['a' => 7, 'n' => 1, 'd' => 239], ['a' => -12, 'n' => 1, 'd' => 682], ['a' => 24, 'n' => 1, 'd' => 12943]], [['a' => 88, 'n' => 1, 'd' => 172], ['a' => 51, 'n' => 1, 'd' => 239], ['a' => 32, 'n' => 1, 'd' => 682], ['a' => 44, 'n' => 1, 'd' => 5357], ['a' => 68, 'n' => 1, 'd' => 12943]], [['a' => 88, 'n' => 1, 'd' => 172], ['a' => 51, 'n' => 1, 'd' => 239], ['a' => 32, 'n' => 1, 'd' => 682], ['a' => 44, 'n' => 1, 'd' => 5357], ['a' => 68, 'n' => 1, 'd' => 12944]]];
function format($ts) {
  global $br, $testCases, $tanEval, $tans;
  $s = '[';
  $i = 0;
  while ($i < count($ts)) {
  $t = $ts[$i];
  $s = $s . '{' . _str($t['a']) . ' ' . _str($t['n']) . ' ' . _str($t['d']) . '}';
  if ($i < count($ts) - 1) {
  $s = $s . ' ';
}
  $i = $i + 1;
};
  return $s . ']';
}
function tanEval($coef, $f) {
  global $br, $testCases, $format, $tans;
  if ($coef == 1) {
  return $f;
}
  if ($coef < 0) {
  return _mul(_bigrat((tanEval(-$coef, $f)), null), -1);
}
  $ca = intdiv($coef, 2);
  $cb = $coef - $ca;
  $a = tanEval($ca, $f);
  $b = tanEval($cb, $f);
  return _div(_bigrat((_add($a, $b)), null), _bigrat((_sub(_bigrat(1, null), _mul($a, $b))), null));
}
function tans($m) {
  global $br, $testCases, $format, $tanEval;
  if (count($m) == 1) {
  $t = $m[0];
  return tanEval($t['a'], br($t['n'], $t['d']));
}
  $half = count($m) / 2;
  $a = tans(array_slice($m, 0, $half - 0));
  $b = tans(array_slice($m, $half));
  return _div(_bigrat((_add($a, $b)), null), _bigrat((_sub(_bigrat(1, null), _mul($a, $b))), null));
}
foreach ($testCases as $ts) {
  echo rtrim('tan ' . format($ts) . ' = ' . _str(tans($ts))), PHP_EOL;
}
