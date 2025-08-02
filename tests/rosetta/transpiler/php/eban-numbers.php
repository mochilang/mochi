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
  $vals = [0, 2, 4, 6, 30, 32, 34, 36, 40, 42, 44, 46, 50, 52, 54, 56, 60, 62, 64, 66];
  $billions = [0, 2, 4, 6];
  function ebanNumbers($start, $stop) {
  global $vals, $billions;
  $nums = [];
  foreach ($billions as $b) {
  foreach ($vals as $m) {
  foreach ($vals as $t) {
  foreach ($vals as $r) {
  $n = $b * 1000000000 + $m * 1000000 + $t * 1000 + $r;
  if (($n >= $start) && ($n <= $stop)) {
  $nums = array_merge($nums, [$n]);
}
};
};
};
};
  return $nums;
};
  function countEban($start, $stop) {
  global $vals, $billions;
  $count = 0;
  foreach ($billions as $b) {
  foreach ($vals as $m) {
  foreach ($vals as $t) {
  foreach ($vals as $r) {
  $n = $b * 1000000000 + $m * 1000000 + $t * 1000 + $r;
  if (($n >= $start) && ($n <= $stop)) {
  $count = $count + 1;
}
};
};
};
};
  return $count;
};
  function main() {
  global $vals, $billions;
  $ranges = [[2, 1000, true], [1000, 4000, true], [2, 10000, false], [2, 100000, false], [2, 1000000, false], [2, 10000000, false], [2, 100000000, false], [2, 1000000000, false]];
  foreach ($ranges as $rg) {
  $start = intval($rg[0]);
  $stop = intval($rg[1]);
  $show = boolval($rg[2]);
  if ($start == 2) {
  echo rtrim('eban numbers up to and including ' . _str($stop) . ':'), PHP_EOL;
} else {
  echo rtrim('eban numbers between ' . _str($start) . ' and ' . _str($stop) . ' (inclusive):'), PHP_EOL;
}
  if ($show) {
  $nums = ebanNumbers($start, $stop);
  $line = '';
  $i = 0;
  while ($i < count($nums)) {
  $line = $line . _str($nums[$i]) . ' ';
  $i = $i + 1;
};
  if (strlen($line) > 0) {
  echo rtrim(substr($line, 0, strlen($line) - 1 - 0)), PHP_EOL;
};
}
  $c = countEban($start, $stop);
  echo rtrim('count = ' . _str($c) . '
'), PHP_EOL;
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
