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
  $PI = 3.141592653589793;
  function sinApprox($x) {
  global $PI;
  $term = $x;
  $sum = $x;
  $n = 1;
  while ($n <= 8) {
  $denom = floatval(((2 * $n) * (2 * $n + 1)));
  $term = -$term * $x * $x / $denom;
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
};
  function cosApprox($x) {
  global $PI;
  $term = 1.0;
  $sum = 1.0;
  $n = 1;
  while ($n <= 8) {
  $denom = floatval(((2 * $n - 1) * (2 * $n)));
  $term = -$term * $x * $x / $denom;
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
};
  function atanApprox($x) {
  global $PI;
  if ($x > 1.0) {
  return $PI / 2.0 - $x / ($x * $x + 0.28);
}
  if ($x < (-1.0)) {
  return -$PI / 2.0 - $x / ($x * $x + 0.28);
}
  return $x / (1.0 + 0.28 * $x * $x);
};
  function atan2Approx($y, $x) {
  global $PI;
  if ($x > 0.0) {
  return atanApprox($y / $x);
}
  if ($x < 0.0) {
  if ($y >= 0.0) {
  return atanApprox($y / $x) + $PI;
};
  return atanApprox($y / $x) - $PI;
}
  if ($y > 0.0) {
  return $PI / 2.0;
}
  if ($y < 0.0) {
  return -$PI / 2.0;
}
  return 0.0;
};
  function digit($ch) {
  global $PI;
  $digits = '0123456789';
  $i = 0;
  while ($i < strlen($digits)) {
  if (substr($digits, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return 0;
};
  function parseTwo($s, $idx) {
  global $PI;
  return digit(substr($s, $idx, $idx + 1 - $idx)) * 10 + digit(substr($s, $idx + 1, $idx + 2 - ($idx + 1)));
};
  function parseSec($s) {
  global $PI;
  $h = parseTwo($s, 0);
  $m = parseTwo($s, 3);
  $sec = parseTwo($s, 6);
  $tmp = ($h * 60 + $m) * 60 + $sec;
  return floatval($tmp);
};
  function pad($n) {
  global $PI;
  if ($n < 10) {
  return '0' . _str($n);
}
  return _str($n);
};
  function meanTime($times) {
  global $PI;
  $ssum = 0.0;
  $csum = 0.0;
  $i = 0;
  while ($i < count($times)) {
  $sec = parseSec($times[$i]);
  $ang = $sec * 2.0 * $PI / 86400.0;
  $ssum = $ssum + sinApprox($ang);
  $csum = $csum + cosApprox($ang);
  $i = $i + 1;
};
  $theta = atan2Approx($ssum, $csum);
  $frac = $theta / (2.0 * $PI);
  while ($frac < 0.0) {
  $frac = $frac + 1.0;
};
  $total = $frac * 86400.0;
  $si = intval($total);
  $h = intval((_intdiv($si, 3600)));
  $m = intval((_intdiv(($si % 3600), 60)));
  $s = intval(($si % 60));
  return pad($h) . ':' . pad($m) . ':' . pad($s);
};
  function main() {
  global $PI;
  $inputs = ['23:00:17', '23:40:20', '00:12:45', '00:17:19'];
  echo rtrim(meanTime($inputs)), PHP_EOL;
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
