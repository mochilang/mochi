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
  function splitPath($p) {
  $parts = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($p)) {
  if (substr($p, $i, $i + 1 - $i) == '/') {
  if ($cur != '') {
  $parts = array_merge($parts, [$cur]);
  $cur = '';
};
} else {
  $cur = $cur . substr($p, $i, $i + 1 - $i);
}
  $i = $i + 1;
};
  if ($cur != '') {
  $parts = array_merge($parts, [$cur]);
}
  return $parts;
};
  function joinPath($parts) {
  $s = '';
  $i = 0;
  while ($i < count($parts)) {
  $s = $s . '/' . $parts[$i];
  $i = $i + 1;
};
  return $s;
};
  function commonPrefix($paths) {
  if (count($paths) == 0) {
  return '';
}
  $base = splitPath($paths[0]);
  $i = 0;
  $prefix = [];
  while ($i < count($base)) {
  $comp = $base[$i];
  $ok = true;
  foreach ($paths as $p) {
  $parts = splitPath($p);
  if ($i >= count($parts) || $parts[$i] != $comp) {
  $ok = false;
  break;
}
};
  if ($ok) {
  $prefix = array_merge($prefix, [$comp]);
} else {
  break;
}
  $i = $i + 1;
};
  return joinPath($prefix);
};
  function main() {
  $paths = ['/home/user1/tmp/coverage/test', '/home/user1/tmp/covert/operator', '/home/user1/tmp/coven/members', '/home//user1/tmp/coventry', '/home/user1/././tmp/covertly/foo', '/home/bob/../user1/tmp/coved/bar'];
  $c = commonPrefix($paths);
  if ($c == '') {
  echo rtrim('No common path'), PHP_EOL;
} else {
  echo rtrim('Common path: ' . $c), PHP_EOL;
}
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
