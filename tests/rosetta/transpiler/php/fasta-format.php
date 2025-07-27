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
  $FASTA = '>Rosetta_Example_1
' . 'THERECANBENOSPACE
' . '>Rosetta_Example_2
' . 'THERECANBESEVERAL
' . 'LINESBUTTHEYALLMUST
' . 'BECONCATENATED';
  function splitLines($s) {
  global $FASTA;
  $lines = [];
  $start = 0;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == '
') {
  $lines = array_merge($lines, [substr($s, $start, $i - $start)]);
  $i = $i + 1;
  $start = $i;
} else {
  $i = $i + 1;
}
};
  $lines = array_merge($lines, [substr($s, $start, strlen($s) - $start)]);
  return $lines;
};
  function parseFasta($text) {
  global $FASTA;
  $key = '';
  $val = '';
  $out = [];
  foreach (splitLines($text) as $line) {
  if ($line == '') {
  continue;
}
  if (substr($line, 0, 1 - 0) == '>') {
  if ($key != '') {
  $out = array_merge($out, [$key . ': ' . $val]);
};
  $hdr = substr($line, 1, strlen($line) - 1);
  $idx = 0;
  while ($idx < strlen($hdr) && substr($hdr, $idx, $idx + 1 - $idx) != ' ') {
  $idx = $idx + 1;
};
  $key = substr($hdr, 0, $idx - 0);
  $val = '';
} else {
  if ($key == '') {
  echo rtrim('missing header'), PHP_EOL;
  return [];
};
  $val = $val . $line;
}
};
  if ($key != '') {
  $out = array_merge($out, [$key . ': ' . $val]);
}
  return $out;
};
  function main() {
  global $FASTA;
  $res = parseFasta($FASTA);
  foreach ($res as $line) {
  echo rtrim($line), PHP_EOL;
};
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
