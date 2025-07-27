<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        return bcadd(strval($a), strval($b), 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        return bcsub(strval($a), strval($b), 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        return bcmul(strval($a), strval($b), 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        return intval(bcmod(strval($a), strval($b)));
    }
    return $a % $b;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function bernoulli($n) {
  $a = [];
  $m = 0;
  while ($m <= $n) {
  $a = array_merge($a, [_div(_bigrat(1, null), (_bigrat(($m + 1), null)))]);
  $j = $m;
  while ($j >= 1) {
  $a[$j - 1] = _mul((_bigrat($j, null)), (_sub($a[$j - 1], $a[$j])));
  $j = $j - 1;
};
  $m = $m + 1;
};
  return $a[0];
};
  function binom($n, $k) {
  if ($k < 0 || $k > $n) {
  return 0;
}
  $kk = $k;
  if ($kk > $n - $kk) {
  $kk = $n - $kk;
}
  $res = 1;
  $i = 0;
  while ($i < $kk) {
  $res = _imul($res, (($n - $i)));
  $i = $i + 1;
  $res = _idiv($res, ($i));
};
  return $res;
};
  function coeff($p, $j) {
  $base = _div(_bigrat(1, null), (_bigrat(($p + 1), null)));
  $c = $base;
  if ($j % 2 == 1) {
  $c = _mul($c, -1);
}
  $c = _mul($c, (_bigrat(binom($p + 1, $j), null)));
  $c = _mul($c, bernoulli($j));
  return $c;
};
  function main() {
  $p = 0;
  while ($p < 10) {
  $line = _str($p) . ' :';
  $j = 0;
  while ($j <= $p) {
  $c = coeff($p, $j);
  if (_str($c) != '0/1') {
  $line = $line . ' ' . _str($c) . '×n';
  $exp = $p + 1 - $j;
  if ($exp > 1) {
  $line = $line . '^' . _str($exp);
};
}
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  $p = $p + 1;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
