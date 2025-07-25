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
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function sameDigits($n, $b) {
  $f = $n % $b;
  $n = intval((_intdiv($n, $b)));
  while ($n > 0) {
  if ($n % $b != $f) {
  return false;
}
  $n = intval((_intdiv($n, $b)));
};
  return true;
};
  function isBrazilian($n) {
  if ($n < 7) {
  return false;
}
  if ($n % 2 == 0 && $n >= 8) {
  return true;
}
  $b = 2;
  while ($b < $n - 1) {
  if (sameDigits($n, $b)) {
  return true;
}
  $b = $b + 1;
};
  return false;
};
  function isPrime($n) {
  if ($n < 2) {
  return false;
}
  if ($n % 2 == 0) {
  return $n == 2;
}
  if ($n % 3 == 0) {
  return $n == 3;
}
  $d = 5;
  while ($d * $d <= $n) {
  if ($n % $d == 0) {
  return false;
}
  $d = $d + 2;
  if ($n % $d == 0) {
  return false;
}
  $d = $d + 4;
};
  return true;
};
  function main() {
  $kinds = [' ', ' odd ', ' prime '];
  foreach ($kinds as $kind) {
  echo rtrim('First 20' . $kind . 'Brazilian numbers:'), PHP_EOL;
  $c = 0;
  $n = 7;
  while (true) {
  if (isBrazilian($n)) {
  echo rtrim(_str($n) . ' '), PHP_EOL;
  $c = $c + 1;
  if ($c == 20) {
  echo rtrim('\n'), PHP_EOL;
  break;
};
}
  if ($kind == ' ') {
  $n = $n + 1;
} else {
  if ($kind == ' odd ') {
  $n = $n + 2;
} else {
  while (true) {
  $n = $n + 2;
  if (isPrime($n)) {
  break;
}
};
};
}
};
};
  $n = 7;
  $c = 0;
  while ($c < 100000) {
  if (isBrazilian($n)) {
  $c = $c + 1;
}
  $n = $n + 1;
};
  echo rtrim('The 100,000th Brazilian number: ' . _str($n - 1)), PHP_EOL;
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
