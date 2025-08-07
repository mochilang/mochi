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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  $DOOMSDAY_LEAP = [4, 1, 7, 4, 2, 6, 4, 1, 5, 3, 7, 5];
  $DOOMSDAY_NOT_LEAP = [3, 7, 7, 4, 2, 6, 4, 1, 5, 3, 7, 5];
  $WEEK_DAY_NAMES = [0 => 'Sunday', 1 => 'Monday', 2 => 'Tuesday', 3 => 'Wednesday', 4 => 'Thursday', 5 => 'Friday', 6 => 'Saturday'];
  function get_week_day($year, $month, $day) {
  global $DOOMSDAY_LEAP, $DOOMSDAY_NOT_LEAP, $WEEK_DAY_NAMES;
  if ($year < 100) {
  _panic('year should be in YYYY format');
}
  if ($month < 1 || $month > 12) {
  _panic('month should be between 1 to 12');
}
  if ($day < 1 || $day > 31) {
  _panic('day should be between 1 to 31');
}
  $century = _intdiv($year, 100);
  $century_anchor = (5 * ($century % 4) + 2) % 7;
  $centurian = $year % 100;
  $centurian_m = $centurian % 12;
  $dooms_day = ((_intdiv($centurian, 12)) + $centurian_m + (_intdiv($centurian_m, 4)) + $century_anchor) % 7;
  $day_anchor = ($year % 4 != 0 || ($centurian == 0 && $year % 400 != 0) ? $DOOMSDAY_NOT_LEAP[$month - 1] : $DOOMSDAY_LEAP[$month - 1]);
  $week_day = ($dooms_day + $day - $day_anchor) % 7;
  if ($week_day < 0) {
  $week_day = $week_day + 7;
}
  return $WEEK_DAY_NAMES[$week_day];
};
  echo rtrim(get_week_day(2020, 10, 24)), PHP_EOL;
  echo rtrim(get_week_day(2017, 10, 24)), PHP_EOL;
  echo rtrim(get_week_day(2019, 5, 3)), PHP_EOL;
  echo rtrim(get_week_day(1970, 9, 16)), PHP_EOL;
  echo rtrim(get_week_day(1870, 8, 13)), PHP_EOL;
  echo rtrim(get_week_day(2040, 3, 14)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
