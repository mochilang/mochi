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
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function lower($ch) {
  global $partList, $nAssemblies, $a;
  $up = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $low = 'abcdefghijklmnopqrstuvwxyz';
  $i = 0;
  while ($i < strlen($up)) {
  if ($ch == substr($up, $i, $i + 1 - $i)) {
  return substr($low, $i, $i + 1 - $i);
}
  $i = $i + 1;
};
  return $ch;
};
  $partList = ['A', 'B', 'C', 'D'];
  $nAssemblies = 3;
  foreach ($partList as $p) {
  echo rtrim($p . ' worker running'), PHP_EOL;
}
  for ($cycle = 1; $cycle < ($nAssemblies + 1); $cycle++) {
  echo rtrim('begin assembly cycle ' . _str($cycle)), PHP_EOL;
  $a = '';
  foreach ($partList as $p) {
  echo rtrim($p . ' worker begins part'), PHP_EOL;
  echo rtrim($p . ' worker completed ' . strtolower($p)), PHP_EOL;
  $a = $a . strtolower($p);
};
  echo rtrim($a . ' assembled.  cycle ' . _str($cycle) . ' complete'), PHP_EOL;
}
  foreach ($partList as $p) {
  echo rtrim($p . ' worker stopped'), PHP_EOL;
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
