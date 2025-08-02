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
  function toOct($n) {
  if ($n == 0) {
  return '0';
}
  $digits = '01234567';
  $out = '';
  $v = $n;
  while ($v > 0) {
  $d = $v % 8;
  $out = substr($digits, $d, $d + 1 - $d) . $out;
  $v = _intdiv($v, 8);
};
  return $out;
};
  function main() {
  $i = 0.0;
  while (true) {
  echo rtrim(toOct(intval($i))), PHP_EOL;
  if ($i == 3.0) {
  $i = 9007199254740992.0 - 4.0;
  echo rtrim('...'), PHP_EOL;
}
  $next = $i + 1.0;
  if ($next == $i) {
  break;
}
  $i = $next;
};
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
