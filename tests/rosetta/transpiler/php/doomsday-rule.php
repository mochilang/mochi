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
function parseIntStr($s, $base = 10) {
    return intval($s, intval($base));
}
$__start_mem = memory_get_usage();
$__start = _now();
  function mochi_parseIntStr($str) {
  global $days, $firstDaysCommon, $firstDaysLeap;
  $i = 0;
  $neg = false;
  if (strlen($str) > 0 && substr($str, 0, 1 - 0) == '-') {
  $neg = true;
  $i = 1;
}
  $n = 0;
  $digits = ['0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9];
  while ($i < strlen($str)) {
  $n = $n * 10 + $digits[substr($str, $i, $i + 1 - $i)];
  $i = $i + 1;
};
  if ($neg) {
  $n = -$n;
}
  return $n;
};
  $days = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'];
  function anchorDay($y) {
  global $days, $firstDaysCommon, $firstDaysLeap;
  return (2 + 5 * ($y % 4) + 4 * ($y % 100) + 6 * ($y % 400)) % 7;
};
  function isLeapYear($y) {
  global $days, $firstDaysCommon, $firstDaysLeap;
  return $y % 4 == 0 && ($y % 100 != 0 || $y % 400 == 0);
};
  $firstDaysCommon = [3, 7, 7, 4, 2, 6, 4, 1, 5, 3, 7, 5];
  $firstDaysLeap = [4, 1, 7, 4, 2, 6, 4, 1, 5, 3, 7, 5];
  function main() {
  global $days, $firstDaysCommon, $firstDaysLeap;
  $dates = ['1800-01-06', '1875-03-29', '1915-12-07', '1970-12-23', '2043-05-14', '2077-02-12', '2101-04-02'];
  echo rtrim('Days of week given by Doomsday rule:'), PHP_EOL;
  foreach ($dates as $date) {
  $y = parseIntStr(substr($date, 0, 4 - 0), 10);
  $m = parseIntStr(substr($date, 5, 7 - 5), 10) - 1;
  $d = parseIntStr(substr($date, 8, 10 - 8), 10);
  $a = anchorDay($y);
  $f = $firstDaysCommon[$m];
  if (isLeapYear($y)) {
  $f = $firstDaysLeap[$m];
}
  $w = $d - $f;
  if ($w < 0) {
  $w = 7 + $w;
}
  $dow = ($a + $w) % 7;
  echo rtrim($date . ' -> ' . $days[$dow]), PHP_EOL;
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
