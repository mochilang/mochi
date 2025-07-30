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
  function weekday($y, $m, $d) {
  $yy = $y;
  $mm = $m;
  if ($mm < 3) {
  $mm = $mm + 12;
  $yy = $yy - 1;
}
  $k = $yy % 100;
  $j = intval((_intdiv($yy, 100)));
  $a = intval((_intdiv((13 * ($mm + 1)), 5)));
  $b = intval((_intdiv($k, 4)));
  $c = intval((_intdiv($j, 4)));
  return ($d + $a + $k + $b + $c + 5 * $j) % 7;
};
  function main() {
  $months31 = [1, 3, 5, 7, 8, 10, 12];
  $names = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'];
  $count = 0;
  $firstY = 0;
  $firstM = 0;
  $lastY = 0;
  $lastM = 0;
  $haveNone = [];
  echo rtrim('Months with five weekends:'), PHP_EOL;
  for ($year = 1900; $year < 2101; $year++) {
  $hasOne = false;
  foreach ($months31 as $m) {
  if (weekday($year, $m, 1) == 6) {
  echo rtrim('  ' . _str($year) . ' ' . $names[$m - 1]), PHP_EOL;
  $count = $count + 1;
  $hasOne = true;
  $lastY = $year;
  $lastM = $m;
  if ($firstY == 0) {
  $firstY = $year;
  $firstM = $m;
};
}
};
  if (!$hasOne) {
  $haveNone = array_merge($haveNone, [$year]);
}
};
  echo rtrim(_str($count) . ' total'), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  echo rtrim('First five dates of weekends:'), PHP_EOL;
  for ($i = 0; $i < 5; $i++) {
  $day = 1 + 7 * $i;
  echo rtrim('  Friday, ' . $names[$firstM - 1] . ' ' . _str($day) . ', ' . _str($firstY)), PHP_EOL;
};
  echo rtrim('Last five dates of weekends:'), PHP_EOL;
  for ($i = 0; $i < 5; $i++) {
  $day = 1 + 7 * $i;
  echo rtrim('  Friday, ' . $names[$lastM - 1] . ' ' . _str($day) . ', ' . _str($lastY)), PHP_EOL;
};
  echo rtrim(''), PHP_EOL;
  echo rtrim('Years with no months with five weekends:'), PHP_EOL;
  foreach ($haveNone as $y) {
  echo rtrim('  ' . _str($y)), PHP_EOL;
};
  echo rtrim(_str(count($haveNone)) . ' total'), PHP_EOL;
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
