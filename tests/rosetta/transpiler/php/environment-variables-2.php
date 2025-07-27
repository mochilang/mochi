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
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
function _environ() {
    $vars = getenv();
    $list = [];
    foreach ($vars as $k => $v) { $list[] = "$k=$v"; }
    return $list;
}
$__start_mem = memory_get_usage();
$__start = _now();
  $os = ['Getenv' => 'getenv', 'Environ' => '_environ'];
  function hasPrefix($s, $p) {
  global $name, $prefix;
  if (strlen($p) > strlen($s)) {
  return false;
}
  return substr($s, 0, strlen($p) - 0) == $p;
};
  $name = 'SHELL';
  $prefix = $name . '=';
  foreach ($os['Environ']() as $v) {
  if (hasPrefix($v, $prefix)) {
  echo rtrim($name . ' has value ' . substr($v, strlen($prefix), _len($v) - strlen($prefix))), PHP_EOL;
  return;
}
}
  echo rtrim($name . ' not found'), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
