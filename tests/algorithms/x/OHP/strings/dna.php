<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
  function is_valid($strand) {
  $i = 0;
  while ($i < strlen($strand)) {
  $ch = substr($strand, $i, $i + 1 - $i);
  if ($ch != 'A' && $ch != 'T' && $ch != 'C' && $ch != 'G') {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function dna($strand) {
  if (!is_valid($strand)) {
  echo rtrim('ValueError: Invalid Strand'), PHP_EOL;
  return '';
}
  $result = '';
  $i = 0;
  while ($i < strlen($strand)) {
  $ch = substr($strand, $i, $i + 1 - $i);
  if ($ch == 'A') {
  $result = $result . 'T';
} else {
  if ($ch == 'T') {
  $result = $result . 'A';
} else {
  if ($ch == 'C') {
  $result = $result . 'G';
} else {
  $result = $result . 'C';
};
};
}
  $i = $i + 1;
};
  return $result;
};
  echo rtrim(dna('GCTA')), PHP_EOL;
  echo rtrim(dna('ATGC')), PHP_EOL;
  echo rtrim(dna('CTGA')), PHP_EOL;
  echo rtrim(dna('GFGG')), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
