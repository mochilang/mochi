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
  function gauss_easter($year) {
  global $years, $i, $y, $e;
  $metonic_cycle = $year % 19;
  $julian_leap_year = $year % 4;
  $non_leap_year = $year % 7;
  $leap_day_inhibits = _intdiv($year, 100);
  $lunar_orbit_correction = _intdiv((13 + 8 * $leap_day_inhibits), 25);
  $leap_day_reinstall_number = (floatval($leap_day_inhibits)) / 4.0;
  $secular_moon_shift = fmod((15.0 - (floatval($lunar_orbit_correction)) + (floatval($leap_day_inhibits)) - $leap_day_reinstall_number), 30.0);
  $century_starting_point = fmod((4.0 + (floatval($leap_day_inhibits)) - $leap_day_reinstall_number), 7.0);
  $days_to_add = fmod((19.0 * (floatval($metonic_cycle)) + $secular_moon_shift), 30.0);
  $days_from_phm_to_sunday = fmod((2.0 * (floatval($julian_leap_year)) + 4.0 * (floatval($non_leap_year)) + 6.0 * $days_to_add + $century_starting_point), 7.0);
  if ($days_to_add == 29.0 && $days_from_phm_to_sunday == 6.0) {
  return ['month' => 4, 'day' => 19];
}
  if ($days_to_add == 28.0 && $days_from_phm_to_sunday == 6.0) {
  return ['month' => 4, 'day' => 18];
}
  $offset = intval(($days_to_add + $days_from_phm_to_sunday));
  $total = 22 + $offset;
  if ($total > 31) {
  return ['month' => 4, 'day' => $total - 31];
}
  return ['month' => 3, 'day' => $total];
};
  function format_date($year, $d) {
  global $years, $i, $y, $e;
  $month = ($d['month'] < 10 ? '0' . _str($d['month']) : _str($d['month']));
  $day = ($d['day'] < 10 ? '0' . _str($d['day']) : _str($d['day']));
  return _str($year) . '-' . $month . '-' . $day;
};
  $years = [1994, 2000, 2010, 2021, 2023, 2032, 2100];
  $i = 0;
  while ($i < count($years)) {
  $y = $years[$i];
  $e = gauss_easter($y);
  echo rtrim('Easter in ' . _str($y) . ' is ' . format_date($y, $e)), PHP_EOL;
  $i = $i + 1;
}
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
