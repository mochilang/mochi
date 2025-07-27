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
  function getDivisors($n) {
  $divs = [1, $n];
  $i = 2;
  while ($i * $i <= $n) {
  if ($n % $i == 0) {
  $j = intval((_intdiv($n, $i)));
  $divs = array_merge($divs, [$i]);
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
  foreach ($xs as $x) {
  $s = $s + $x;
};
  return $s;
};
  function isPartSum($divs, $target) {
  $possible = [];
  $i = 0;
  while ($i <= $target) {
  $possible = array_merge($possible, [false]);
  $i = $i + 1;
};
  $possible[0] = true;
  foreach ($divs as $v) {
  $s = $target;
  while ($s >= $v) {
  if ($possible[$s - $v]) {
  $possible[$s] = true;
}
  $s = $s - 1;
};
};
  return $possible[$target];
};
  function isZumkeller($n) {
  $divs = getDivisors($n);
  $s = array_sum($divs);
  if ($s % 2 == 1) {
  return false;
}
  if ($n % 2 == 1) {
  $abundance = $s - 2 * $n;
  return $abundance > 0 && $abundance % 2 == 0;
}
  return isPartSum($divs, _intdiv($s, 2));
};
  function pad($n, $width) {
  $s = _str($n);
  while (strlen($s) < $width) {
  $s = ' ' . $s;
};
  return $s;
};
  function main() {
  echo rtrim('The first 220 Zumkeller numbers are:'), PHP_EOL;
  $count = 0;
  $line = '';
  $i = 2;
  while ($count < 220) {
  if (isZumkeller($i)) {
  $line = $line . pad($i, 3) . ' ';
  $count = $count + 1;
  if ($count % 20 == 0) {
  echo rtrim(substr($line, 0, strlen($line) - 1 - 0)), PHP_EOL;
  $line = '';
};
}
  $i = $i + 1;
};
  echo rtrim('
The first 40 odd Zumkeller numbers are:'), PHP_EOL;
  $count = 0;
  $line = '';
  $i = 3;
  while ($count < 40) {
  if (isZumkeller($i)) {
  $line = $line . pad($i, 5) . ' ';
  $count = $count + 1;
  if ($count % 10 == 0) {
  echo rtrim(substr($line, 0, strlen($line) - 1 - 0)), PHP_EOL;
  $line = '';
};
}
  $i = $i + 2;
};
  echo rtrim('
The first 40 odd Zumkeller numbers which don\'t end in 5 are:'), PHP_EOL;
  $count = 0;
  $line = '';
  $i = 3;
  while ($count < 40) {
  if ($i % 10 != 5 && isZumkeller($i)) {
  $line = $line . pad($i, 7) . ' ';
  $count = $count + 1;
  if ($count % 8 == 0) {
  echo rtrim(substr($line, 0, strlen($line) - 1 - 0)), PHP_EOL;
  $line = '';
};
}
  $i = $i + 2;
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
