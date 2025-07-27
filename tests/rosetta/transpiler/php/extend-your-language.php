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
  function else1($i, $f) {
  global $a, $b, $t;
  if ($i['cond1'] && ($i['cond2'] == false)) {
  $f();
}
  return $i;
};
  function else2($i, $f) {
  global $a, $b, $t;
  if ($i['cond2'] && ($i['cond1'] == false)) {
  $f();
}
  return $i;
};
  function else0($i, $f) {
  global $a, $b, $t;
  if (($i['cond1'] == false) && ($i['cond2'] == false)) {
  $f();
}
  return $i;
};
  function if2($cond1, $cond2, $f) {
  global $a, $b, $t;
  if ($cond1 && $cond2) {
  $f();
}
  return ['cond1' => $cond1, 'cond2' => $cond2];
};
  $a = 0;
  $b = 1;
  $t = if2($a == 1, $b == 3, function() {
  echo rtrim('a = 1 and b = 3'), PHP_EOL;
});
  $t = else1($t, function() {
  echo rtrim('a = 1 and b <> 3'), PHP_EOL;
});
  $t = else2($t, function() {
  echo rtrim('a <> 1 and b = 3'), PHP_EOL;
});
  else0($t, function() {
  echo rtrim('a <> 1 and b <> 3'), PHP_EOL;
});
  $a = 1;
  $b = 0;
  $t = if2($a == 1, $b == 3, function() {
  echo rtrim('a = 1 and b = 3'), PHP_EOL;
});
  $t = else0($t, function() {
  echo rtrim('a <> 1 and b <> 3'), PHP_EOL;
});
  else1($t, function() {
  echo rtrim('a = 1 and b <> 3'), PHP_EOL;
});
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
