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
$__start_mem = memory_get_usage();
$__start = _now();
  function bernoulli($n) {
  global $b, $numStr, $denStr;
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
  function padStart($s, $width, $pad) {
  global $b, $numStr, $denStr;
  $out = $s;
  while (strlen($out) < $width) {
  $out = $pad . $out;
};
  return $out;
};
  for ($i = 0; $i < 61; $i++) {
  $b = bernoulli($i);
  if (num($b) != 0) {
  $numStr = _str(num($b));
  $denStr = _str(denom($b));
  echo rtrim('B(' . str_pad(_str($i), 2, ' ', STR_PAD_LEFT) . ') =' . str_pad($numStr, 45, ' ', STR_PAD_LEFT) . '/' . $denStr), PHP_EOL;
}
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
