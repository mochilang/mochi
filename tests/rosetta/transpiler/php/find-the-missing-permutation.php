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
$__start_mem = memory_get_usage();
$__start = _now();
  $given = ['ABCD', 'CABD', 'ACDB', 'DACB', 'BCDA', 'ACBD', 'ADCB', 'CDAB', 'DABC', 'BCAD', 'CADB', 'CDBA', 'CBAD', 'ABDC', 'ADBC', 'BDCA', 'DCBA', 'BACD', 'BADC', 'BDAC', 'CBDA', 'DBCA', 'DCAB'];
  function idx($ch) {
  global $given;
  if ($ch == 'A') {
  return 0;
}
  if ($ch == 'B') {
  return 1;
}
  if ($ch == 'C') {
  return 2;
}
  return 3;
};
  function main() {
  global $given;
  $res = '';
  $i = 0;
  while ($i < strlen($given[0])) {
  $counts = [0, 0, 0, 0];
  foreach ($given as $p) {
  $ch = substr($p, $i, $i + 1 - $i);
  $j = idx($ch);
  $counts[$j] = $counts[$j] + 1;
};
  $j = 0;
  while ($j < 4) {
  if (fmod($counts[$j], 2) == 1) {
  if ($j == 0) {
  $res = $res . 'A';
} else {
  if ($j == 1) {
  $res = $res . 'B';
} else {
  if ($j == 2) {
  $res = $res . 'C';
} else {
  $res = $res . 'D';
};
};
};
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  echo rtrim($res), PHP_EOL;
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
