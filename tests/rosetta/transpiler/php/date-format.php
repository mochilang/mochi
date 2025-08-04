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
  function pad2($n) {
  if ($n < 10) {
  return '0' . _str($n);
}
  return _str($n);
};
  function weekdayName($z) {
  $names = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'];
  return $names[($z + 4) % 7];
};
  function main() {
  $ts = intval((_now() / 1000000000));
  $days = intval((_intdiv($ts, 86400)));
  $z = $days + 719468;
  $era = intval((_intdiv($z, 146097)));
  $doe = $z - $era * 146097;
  $yoe = _intdiv(($doe - _intdiv($doe, 1460) + _intdiv($doe, 36524) - _intdiv($doe, 146096)), intval(365));
  $y = $yoe + $era * 400;
  $doy = $doe - (365 * $yoe + _intdiv($yoe, 4) - _intdiv($yoe, 100));
  $mp = _intdiv((5 * $doy + 2), intval(153));
  $d = intval(($doy - (_intdiv((153 * $mp + 2), intval(5))) + 1));
  $m = intval(($mp + 3));
  if ($m > 12) {
  $y = $y + 1;
  $m = $m - 12;
}
  $iso = _str($y) . '-' . pad2($m) . '-' . pad2($d);
  echo rtrim($iso), PHP_EOL;
  $months = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'];
  $line = weekdayName($days) . ', ' . $months[$m - 1] . ' ' . _str($d) . ', ' . _str($y);
  echo rtrim($line), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
