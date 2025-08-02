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
  function timeStr($sec) {
  $wks = _intdiv($sec, 604800);
  $sec = $sec % 604800;
  $ds = _intdiv($sec, 86400);
  $sec = $sec % 86400;
  $hrs = _intdiv($sec, 3600);
  $sec = $sec % 3600;
  $mins = _intdiv($sec, 60);
  $sec = $sec % 60;
  $res = '';
  $comma = false;
  if ($wks != 0) {
  $res = $res . _str($wks) . ' wk';
  $comma = true;
}
  if ($ds != 0) {
  if ($comma) {
  $res = $res . ', ';
};
  $res = $res . _str($ds) . ' d';
  $comma = true;
}
  if ($hrs != 0) {
  if ($comma) {
  $res = $res . ', ';
};
  $res = $res . _str($hrs) . ' hr';
  $comma = true;
}
  if ($mins != 0) {
  if ($comma) {
  $res = $res . ', ';
};
  $res = $res . _str($mins) . ' min';
  $comma = true;
}
  if ($sec != 0) {
  if ($comma) {
  $res = $res . ', ';
};
  $res = $res . _str($sec) . ' sec';
}
  return $res;
};
  echo rtrim(timeStr(7259)), PHP_EOL;
  echo rtrim(timeStr(86400)), PHP_EOL;
  echo rtrim(timeStr(6000000)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
