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
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function floorf($x) {
  $y = intval($x);
  return floatval($y);
};
  function indexOf($s, $ch) {
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function fmtF3($x) {
  $y = floorf($x * 1000.0 + 0.5) / 1000.0;
  $s = _str($y);
  $dot = _indexof($s, '.');
  if ($dot == 0 - 1) {
  $s = $s . '.000';
} else {
  $decs = strlen($s) - $dot - 1;
  if ($decs > 3) {
  $s = substr($s, 0, $dot + 4 - 0);
} else {
  while ($decs < 3) {
  $s = $s . '0';
  $decs = $decs + 1;
};
};
}
  return $s;
};
  function padFloat3($x, $width) {
  $s = fmtF3($x);
  while (strlen($s) < $width) {
  $s = ' ' . $s;
};
  return $s;
};
  function fib1000() {
  $a = 0.0;
  $b = 1.0;
  $res = [];
  $i = 0;
  while ($i < 1000) {
  $res = array_merge($res, [$b]);
  $t = $b;
  $b = $b + $a;
  $a = $t;
  $i = $i + 1;
};
  return $res;
};
  function leadingDigit($x) {
  if ($x < 0.0) {
  $x = -$x;
}
  while ($x >= 10.0) {
  $x = $x / 10.0;
};
  while ($x > 0.0 && $x < 1.0) {
  $x = $x * 10.0;
};
  return intval($x);
};
  function show($nums, $title) {
  $counts = [0, 0, 0, 0, 0, 0, 0, 0, 0];
  foreach ($nums as $n) {
  $d = leadingDigit($n);
  if ($d >= 1 && $d <= 9) {
  $counts[$d - 1] = $counts[$d - 1] + 1;
}
};
  $preds = [0.301, 0.176, 0.125, 0.097, 0.079, 0.067, 0.058, 0.051, 0.046];
  $total = count($nums);
  echo rtrim($title), PHP_EOL;
  echo rtrim('Digit  Observed  Predicted'), PHP_EOL;
  $i = 0;
  while ($i < 9) {
  $obs = (floatval($counts[$i])) / (floatval($total));
  $line = '  ' . _str($i + 1) . '  ' . padFloat3($obs, 9) . '  ' . padFloat3($preds[$i], 8);
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
};
};
  function main() {
  show(fib1000(), 'First 1000 Fibonacci numbers');
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
