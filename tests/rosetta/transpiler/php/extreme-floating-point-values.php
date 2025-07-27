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
  function makeInf() {
  $x = 1.0;
  $i = 0;
  while ($i < 400) {
  $x = $x * 10.0;
  $i = $i + 1;
};
  return $x;
};
  function makeMax() {
  $x = 1.0;
  $i = 0;
  while ($i < 308) {
  $x = $x * 10.0;
  $i = $i + 1;
};
  return $x;
};
  function isNaN($x) {
  return $x != $x;
};
  function validateNaN($n, $op) {
  if (isNaN($n)) {
  echo rtrim($op . ' -> NaN'), PHP_EOL;
} else {
  echo rtrim('!!! Expected NaN from') . " " . rtrim($op) . " " . rtrim(' Found') . " " . rtrim(json_encode($n, 1344)), PHP_EOL;
}
};
  function validateZero($n, $op) {
  if ($n == 0) {
  echo rtrim($op . ' -> 0'), PHP_EOL;
} else {
  echo rtrim('!!! Expected 0 from') . " " . rtrim($op) . " " . rtrim(' Found') . " " . rtrim(json_encode($n, 1344)), PHP_EOL;
}
};
  function validateGT($a, $b, $op) {
  if ($a > $b) {
  echo rtrim($op), PHP_EOL;
} else {
  echo rtrim('!!! Expected') . " " . rtrim($op) . " " . rtrim(' Found not true.'), PHP_EOL;
}
};
  function validateNE($a, $b, $op) {
  if ($a == $b) {
  echo rtrim('!!! Expected') . " " . rtrim($op) . " " . rtrim(' Found not true.'), PHP_EOL;
} else {
  echo rtrim($op), PHP_EOL;
}
};
  function validateEQ($a, $b, $op) {
  if ($a == $b) {
  echo rtrim($op), PHP_EOL;
} else {
  echo rtrim('!!! Expected') . " " . rtrim($op) . " " . rtrim(' Found not true.'), PHP_EOL;
}
};
  function main() {
  $negZero = -0.0;
  $posInf = makeInf();
  $negInf = -$posInf;
  $nan = $posInf / $posInf;
  $maxVal = makeMax();
  echo rtrim(json_encode($negZero, 1344)) . " " . rtrim(json_encode($posInf, 1344)) . " " . rtrim(json_encode($negInf, 1344)) . " " . rtrim(json_encode($nan, 1344)), PHP_EOL;
  echo rtrim(json_encode($negZero, 1344)) . " " . rtrim(json_encode($posInf, 1344)) . " " . rtrim(json_encode($negInf, 1344)) . " " . rtrim(json_encode($nan, 1344)), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  validateNaN($negInf + $posInf, '-Inf + Inf');
  validateNaN(0.0 * $posInf, '0 * Inf');
  validateNaN($posInf / $posInf, 'Inf / Inf');
  validateNaN(fmod($posInf, 1.0), 'Inf % 1');
  validateNaN(1.0 + $nan, '1 + NaN');
  validateZero(1.0 / $posInf, '1 / Inf');
  validateGT($posInf, $maxVal, 'Inf > max value');
  validateGT(-$maxVal, $negInf, '-Inf < max neg value');
  validateNE($nan, $nan, 'NaN != NaN');
  validateEQ($negZero, 0.0, '-0 == 0');
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
