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
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
function parseIntStr($s, $base = 10) {
    return intval($s, intval($base));
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
  function mochi_ord($ch) {
  global $candidates, $b;
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  if ($ch >= '0' && $ch <= '9') {
  return parseIntStr($ch, 10) + 48;
}
  $idx = _indexof($upper, $ch);
  if ($idx >= 0) {
  return 65 + $idx;
}
  return 0;
};
  function isCusip($s) {
  global $candidates, $b;
  if (strlen($s) != 9) {
  return false;
}
  $sum = 0;
  $i = 0;
  while ($i < 8) {
  $c = substr($s, $i, $i + 1 - $i);
  $v = 0;
  if ($c >= '0' && $c <= '9') {
  $v = parseIntStr($c, 10);
} else {
  if ($c >= 'A' && $c <= 'Z') {
  $v = mochi_ord($c) - 55;
} else {
  if ($c == '*') {
  $v = 36;
} else {
  if ($c == '@') {
  $v = 37;
} else {
  if ($c == '#') {
  $v = 38;
} else {
  return false;
};
};
};
};
}
  if ($i % 2 == 1) {
  $v = $v * 2;
}
  $sum = $sum + _intdiv($v, 10) + $v % 10;
  $i = $i + 1;
};
  return parseIntStr(substr($s, 8, 9 - 8), 10) == (10 - ($sum % 10)) % 10;
};
  $candidates = ['037833100', '17275R102', '38259P508', '594918104', '68389X106', '68389X105'];
  foreach ($candidates as $cand) {
  $b = 'incorrect';
  if (isCusip($cand)) {
  $b = 'correct';
}
  echo rtrim($cand . ' -> ' . $b), PHP_EOL;
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
