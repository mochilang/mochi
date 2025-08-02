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
  function parseInt($str) {
  global $value, $done, $line, $ans;
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
  function rand10000() {
  global $value, $done, $line, $ans;
  return fmod(_now(), 10000);
};
  $value = 0;
  echo rtrim('Value:') . " " . rtrim(json_encode($value, 1344)), PHP_EOL;
  $done = false;
  while (!$done) {
  echo rtrim('i=increment, r=random, s num=set, q=quit:'), PHP_EOL;
  $line = trim(fgets(STDIN));
  if ($line == 'i') {
  $value = $value + 1;
  echo rtrim('Value:') . " " . rtrim(json_encode($value, 1344)), PHP_EOL;
} else {
  if ($line == 'r') {
  echo rtrim('Set random value? (y/n)'), PHP_EOL;
  $ans = trim(fgets(STDIN));
  if ($ans == 'y') {
  $value = rand10000();
  echo rtrim('Value:') . " " . rtrim(json_encode($value, 1344)), PHP_EOL;
};
} else {
  if (strlen($line) > 2 && substr($line, 0, 2 - 0) == 's ') {
  $value = parseInt(substr($line, 2, strlen($line) - 2));
  echo rtrim('Value:') . " " . rtrim(json_encode($value, 1344)), PHP_EOL;
} else {
  if ($line == 'q') {
  $done = true;
} else {
  echo rtrim('Unknown command'), PHP_EOL;
};
};
};
}
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
