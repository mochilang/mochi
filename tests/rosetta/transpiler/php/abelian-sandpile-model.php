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
  $dim = 16;
  function newPile($d) {
  global $dim;
  $b = [];
  $y = 0;
  while ($y < $d) {
  $row = [];
  $x = 0;
  while ($x < $d) {
  $row = array_merge($row, [0]);
  $x = $x + 1;
};
  $b = array_merge($b, [$row]);
  $y = $y + 1;
};
  return $b;
};
  function handlePile(&$pile, $x, $y) {
  global $dim;
  if ($pile[$y][$x] >= 4) {
  $pile[$y][$x] = $pile[$y][$x] - 4;
  if ($y > 0) {
  $pile[$y - 1][$x] = $pile[$y - 1][$x] + 1;
  if ($pile[$y - 1][$x] >= 4) {
  $pile = handlePile($pile, $x, $y - 1);
};
};
  if ($x > 0) {
  $pile[$y][$x - 1] = $pile[$y][$x - 1] + 1;
  if ($pile[$y][$x - 1] >= 4) {
  $pile = handlePile($pile, $x - 1, $y);
};
};
  if ($y < $dim - 1) {
  $pile[$y + 1][$x] = $pile[$y + 1][$x] + 1;
  if ($pile[$y + 1][$x] >= 4) {
  $pile = handlePile($pile, $x, $y + 1);
};
};
  if ($x < $dim - 1) {
  $pile[$y][$x + 1] = $pile[$y][$x + 1] + 1;
  if ($pile[$y][$x + 1] >= 4) {
  $pile = handlePile($pile, $x + 1, $y);
};
};
  $pile = handlePile($pile, $x, $y);
}
  return $pile;
};
  function drawPile($pile, $d) {
  global $dim;
  $chars = [' ', '░', '▓', '█'];
  $row = 0;
  while ($row < $d) {
  $line = '';
  $col = 0;
  while ($col < $d) {
  $v = $pile[$row][$col];
  if ($v > 3) {
  $v = 3;
}
  $line = $line . $chars[$v];
  $col = $col + 1;
};
  echo rtrim($line), PHP_EOL;
  $row = $row + 1;
};
};
  function main() {
  global $dim;
  $pile = newPile(16);
  $hdim = 7;
  $pile[$hdim][$hdim] = 16;
  $pile = handlePile($pile, $hdim, $hdim);
  drawPile($pile, 16);
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
