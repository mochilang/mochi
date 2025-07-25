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
$__start_mem = memory_get_usage();
$__start = _now();
  $daysInMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  $start = [3, 6, 6, 2, 4, 0, 2, 5, 1, 3, 6, 1];
  $months = [' January ', ' February', '  March  ', '  April  ', '   May   ', '   June  ', '   July  ', '  August ', 'September', ' October ', ' November', ' December'];
  $days = ['Su', 'Mo', 'Tu', 'We', 'Th', 'Fr', 'Sa'];
  echo rtrim('                                [SNOOPY]\n'), PHP_EOL;
  echo rtrim('                                  1969\n'), PHP_EOL;
  $qtr = 0;
  while ($qtr < 4) {
  $mi = 0;
  while ($mi < 3) {
  echo rtrim('      ' . $months[$qtr * 3 + $mi] . '           ') . " " . rtrim((false ? 'true' : 'false')), PHP_EOL;
  $mi = $mi + 1;
};
  echo rtrim(''), PHP_EOL;
  $mi = 0;
  while ($mi < 3) {
  $d = 0;
  while ($d < 7) {
  echo rtrim(' ' . $days[$d]) . " " . rtrim((false ? 'true' : 'false')), PHP_EOL;
  $d = $d + 1;
};
  echo rtrim('     ') . " " . rtrim((false ? 'true' : 'false')), PHP_EOL;
  $mi = $mi + 1;
};
  echo rtrim(''), PHP_EOL;
  $week = 0;
  while ($week < 6) {
  $mi = 0;
  while ($mi < 3) {
  $day = 0;
  while ($day < 7) {
  $m = $qtr * 3 + $mi;
  $val = $week * 7 + $day - $start[$m] + 1;
  if ($val >= 1 && $val <= $daysInMonth[$m]) {
  $s = _str($val);
  if (strlen($s) == 1) {
  $s = ' ' . $s;
};
  echo rtrim(' ' . $s) . " " . rtrim((false ? 'true' : 'false')), PHP_EOL;
} else {
  echo rtrim('   ') . " " . rtrim((false ? 'true' : 'false')), PHP_EOL;
}
  $day = $day + 1;
};
  echo rtrim('     ') . " " . rtrim((false ? 'true' : 'false')), PHP_EOL;
  $mi = $mi + 1;
};
  echo rtrim(''), PHP_EOL;
  $week = $week + 1;
};
  echo rtrim(''), PHP_EOL;
  $qtr = $qtr + 1;
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
