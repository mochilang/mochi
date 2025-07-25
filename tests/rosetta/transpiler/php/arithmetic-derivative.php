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
function repeat($s, $n) {
    return str_repeat($s, intval($n));
}
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function primeFactors($n) {
  $factors = [];
  $x = $n;
  while ($x % 2 == 0) {
  $factors = array_merge($factors, [2]);
  $x = intval((_intdiv($x, 2)));
};
  $p = 3;
  while ($p * $p <= $x) {
  while ($x % $p == 0) {
  $factors = array_merge($factors, [$p]);
  $x = intval((_intdiv($x, $p)));
};
  $p = $p + 2;
};
  if ($x > 1) {
  $factors = array_merge($factors, [$x]);
}
  return $factors;
};
  function mochi_repeat($ch, $n) {
  $s = '';
  $i = 0;
  while ($i < $n) {
  $s = $s . $ch;
  $i = $i + 1;
};
  return $s;
};
  function D($n) {
  if ($n < 0.0) {
  return -D(-$n);
}
  if ($n < 2.0) {
  return 0.0;
}
  $factors = [];
  if ($n < 10000000000000000000.0) {
  $factors = primeFactors(intval(($n)));
} else {
  $g = intval(($n / 100.0));
  $factors = primeFactors($g);
  $factors = array_merge($factors, [2]);
  $factors = array_merge($factors, [2]);
  $factors = array_merge($factors, [5]);
  $factors = array_merge($factors, [5]);
}
  $c = count($factors);
  if ($c == 1) {
  return 1.0;
}
  if ($c == 2) {
  return floatval(($factors[0] + $factors[1]));
}
  $d = $n / (floatval($factors[0]));
  return D($d) * (floatval($factors[0])) + $d;
};
  function pad($n) {
  $s = _str($n);
  while (strlen($s) < 4) {
  $s = ' ' . $s;
};
  return $s;
};
  function main() {
  $vals = [];
  $n = -99;
  while ($n < 101) {
  $vals = array_merge($vals, [intval((D(floatval($n))))]);
  $n = $n + 1;
};
  $i = 0;
  while ($i < count($vals)) {
  $line = '';
  $j = 0;
  while ($j < 10) {
  $line = $line . pad($vals[$i + $j]);
  if ($j < 9) {
  $line = $line . ' ';
}
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  $i = $i + 10;
};
  $pow = 1.0;
  $m = 1;
  while ($m < 21) {
  $pow = $pow * 10.0;
  $exp = _str($m);
  if (strlen($exp) < 2) {
  $exp = $exp . ' ';
}
  $res = _str($m) . repeat('0', $m - 1);
  echo rtrim('D(10^' . $exp . ') / 7 = ' . $res), PHP_EOL;
  $m = $m + 1;
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
