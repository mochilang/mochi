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
  function Node($value, $next, $prev) {
  return ['value' => $value, 'next' => $next, 'prev' => $prev];
};
  function main() {
  $a = Node('A', null, null);
  $b = Node('B', null, $a);
  $a['next'] = $b;
  $c = Node('C', null, $b);
  $b['next'] = $c;
  $p = $a;
  $line = '';
  while ($p != null) {
  $line = $line . (strval($p['value']));
  $p = $p['next'];
  if ($p != null) {
  $line = $line . ' ';
}
};
  echo rtrim($line), PHP_EOL;
  $p = $c;
  $line = '';
  while ($p != null) {
  $line = $line . (strval($p['value']));
  $p = $p['prev'];
  if ($p != null) {
  $line = $line . ' ';
}
};
  echo rtrim($line), PHP_EOL;
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
