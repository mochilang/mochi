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
  function removeName($names, $name) {
  $out = [];
  foreach ($names as $n) {
  if ($n != $name) {
  $out = array_merge($out, [$n]);
}
};
  return $out;
};
  function main() {
  $clients = [];
  $broadcast = function($msg) use (&$broadcast, $clients) {
  echo rtrim($msg), PHP_EOL;
};
  $add = function($name) use (&$add, $clients, $broadcast) {
  $clients = array_merge($clients, [$name]);
  $broadcast('+++ "' . $name . '" connected +++\n');
};
  $send = function($name, $msg) use (&$send, $clients, $broadcast, $add) {
  $broadcast($name . '> ' . $msg . '\n');
};
  $remove = function($name) use (&$remove, $clients, $broadcast, $add, $send) {
  $clients = removeName($clients, $name);
  $broadcast('--- "' . $name . '" disconnected ---\n');
};
  $add('Alice');
  $add('Bob');
  $send('Alice', 'Hello Bob!');
  $send('Bob', 'Hi Alice!');
  $remove('Bob');
  $remove('Alice');
  $broadcast('Server stopping!\n');
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
