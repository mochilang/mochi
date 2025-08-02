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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function gcd($a, $b) {
  $x = $a;
  if ($x < 0) {
  $x = -$x;
}
  $y = $b;
  if ($y < 0) {
  $y = -$y;
}
  while ($y != 0) {
  $t = $x % $y;
  $x = $y;
  $y = $t;
};
  return $x;
};
  function divisors($n) {
  $divs = [];
  $i = 1;
  while ($i * $i <= $n) {
  if ($n % $i == 0) {
  $divs = array_merge($divs, [$i]);
  $j = intval((_intdiv($n, $i)));
  if ($i != $j) {
  $divs = array_merge($divs, [$j]);
};
}
  $i = $i + 1;
};
  return $divs;
};
  function sum($xs) {
  $s = 0;
  foreach ($xs as $v) {
  $s = $s + $v;
};
  return $s;
};
  function isDuffinian($n) {
  $divs = divisors($n);
  if (count($divs) <= 2) {
  return false;
}
  $sigma = array_sum($divs);
  return gcd($sigma, $n) == 1;
};
  function pad($n, $width) {
  $s = _str($n);
  while (strlen($s) < $width) {
  $s = ' ' . $s;
};
  return $s;
};
  function printTable($nums, $perRow, $width) {
  $i = 0;
  $line = '';
  while ($i < count($nums)) {
  $line = $line . ' ' . pad($nums[$i], $width);
  if (($i + 1) % $perRow == 0) {
  echo rtrim(substr($line, 1, strlen($line) - 1)), PHP_EOL;
  $line = '';
}
  $i = $i + 1;
};
  if (strlen($line) > 0) {
  echo rtrim(substr($line, 1, strlen($line) - 1)), PHP_EOL;
}
};
  function main() {
  $duff = [];
  $n = 1;
  while (count($duff) < 50) {
  if (isDuffinian($n)) {
  $duff = array_merge($duff, [$n]);
}
  $n = $n + 1;
};
  echo rtrim('First 50 Duffinian numbers:'), PHP_EOL;
  printTable($duff, 10, 3);
  $triplets = [];
  $n = 1;
  while (count($triplets) < 20) {
  if (isDuffinian($n) && isDuffinian($n + 1) && isDuffinian($n + 2)) {
  $triplets = array_merge($triplets, ['(' . _str($n) . ',' . _str($n + 1) . ',' . _str($n + 2) . ')']);
  $n = $n + 3;
}
  $n = $n + 1;
};
  echo rtrim('
First 20 Duffinian triplets:'), PHP_EOL;
  $i = 0;
  while ($i < count($triplets)) {
  $line = '';
  $j = 0;
  while ($j < 4 && $i < count($triplets)) {
  $line = $line . padStr($triplets[$i], 16);
  $j = $j + 1;
  $i = $i + 1;
};
  echo rtrim($line), PHP_EOL;
};
};
  function padStr($s, $width) {
  $res = $s;
  while (strlen($res) < $width) {
  $res = $res . ' ';
};
  return $res;
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
