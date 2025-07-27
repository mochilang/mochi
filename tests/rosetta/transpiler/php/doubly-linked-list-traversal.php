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
  $nodes = [];
  $head = 0 - 1;
  $tail = 0 - 1;
  function listString() {
  global $nodes, $head, $tail, $out;
  if ($head == 0 - 1) {
  return '<nil>';
}
  $r = '[' . $nodes[$head]['value'];
  $id = intval($nodes[$head]['next']);
  while ($id != 0 - 1) {
  $r = $r . ' ' . $nodes[$id]['value'];
  $id = intval($nodes[$id]['next']);
};
  $r = $r . ']';
  return $r;
};
  echo rtrim(listString()), PHP_EOL;
  $nodes[0] = ['value' => 'A', 'next' => 0 - 1, 'prev' => 0 - 1];
  $head = 0;
  $tail = 0;
  $nodes[1] = ['value' => 'B', 'next' => 0 - 1, 'prev' => 0];
  $nodes[0]['next'] = 1;
  $tail = 1;
  echo rtrim(listString()), PHP_EOL;
  $nodes[2] = ['value' => 'C', 'next' => 1, 'prev' => 0];
  $nodes[1]['prev'] = 2;
  $nodes[0]['next'] = 2;
  echo rtrim(listString()), PHP_EOL;
  $out = 'From tail:';
  $id = $tail;
  while ($id != 0 - 1) {
  $out = $out . ' ' . $nodes[$id]['value'];
  $id = intval($nodes[$id]['prev']);
}
  echo rtrim($out), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
