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
  function copyList($src) {
  global $c1, $c2;
  $out = [];
  foreach ($src as $v) {
  $out = array_merge($out, [$v]);
};
  return $out;
};
  function copyMap($src) {
  global $c1, $c2;
  $out = [];
  foreach (array_keys($src) as $k) {
  $out[$k] = $src[$k];
};
  return $out;
};
  function deepcopy($c) {
  global $c1, $c2;
  return ['i' => $c['i'], 's' => $c['s'], 'b' => copyList($c['b']), 'm' => copyMap($c['m'])];
};
  function cdsStr($c) {
  global $c1, $c2;
  $bs = '[';
  $i = 0;
  while ($i < _len($c['b'])) {
  $bs = $bs . _str($c['b'][$i]);
  if ($i < _len($c['b']) - 1) {
  $bs = $bs . ' ';
}
  $i = $i + 1;
};
  $bs = $bs . ']';
  $ms = 'map[';
  $first = true;
  foreach (array_keys($c['m']) as $k) {
  if (!$first) {
  $ms = $ms . ' ';
}
  $ms = $ms . _str($k) . ':' . _str($c['m'][$k]);
  $first = false;
};
  $ms = $ms . ']';
  return '{' . _str($c['i']) . ' ' . $c['s'] . ' ' . $bs . ' ' . $ms . '}';
};
  $c1 = ['i' => 1, 's' => 'one', 'b' => [117, 110, 105, 116], 'm' => [1 => true]];
  $c2 = deepcopy($c1);
  echo rtrim(cdsStr($c1)), PHP_EOL;
  echo rtrim(cdsStr($c2)), PHP_EOL;
  $c1 = ['i' => 0, 's' => 'nil', 'b' => [122, 101, 114, 111], 'm' => [1 => false]];
  echo rtrim(cdsStr($c1)), PHP_EOL;
  echo rtrim(cdsStr($c2)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
