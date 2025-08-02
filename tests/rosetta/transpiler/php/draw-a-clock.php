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
  function pow2($exp) {
  global $t, $sec, $mins, $min, $hour, $xs, $out, $j;
  $r = 1;
  $i = 0;
  while ($i < $exp) {
  $r = $r * 2;
  $i = $i + 1;
};
  return $r;
};
  function bin($n, $digits) {
  global $t, $sec, $mins, $min, $hour, $xs, $out, $j;
  $s = '';
  $i = $digits - 1;
  while ($i >= 0) {
  $p = pow2($i);
  if ($n >= $p) {
  $s = $s . 'x';
  $n = $n - $p;
} else {
  $s = $s . ' ';
}
  if ($i > 0) {
  $s = $s . '|';
}
  $i = $i - 1;
};
  return $s;
};
  $t = _now() / 1000000000;
  $sec = $t % 60;
  $mins = _intdiv($t, 60);
  $min = $mins % 60;
  $hour = (_intdiv($mins, 60)) % 24;
  echo rtrim(bin($hour, 8)), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  echo rtrim(bin($min, 8)), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  $xs = '';
  $i = 0;
  while ($i < $sec) {
  $xs = $xs . 'x';
  $i = $i + 1;
}
  $out = '';
  $j = 0;
  while ($j < strlen($xs)) {
  $out = $out . substr($xs, $j, $j + 1 - $j);
  if (($j + 1) % 5 == 0 && $j + 1 < strlen($xs)) {
  $out = $out . '|';
}
  $j = $j + 1;
}
  echo rtrim($out), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
