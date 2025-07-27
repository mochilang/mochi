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
  function sqrtApprox($x) {
  if ($x <= 0.0) {
  return 0.0;
}
  $g = $x;
  $i = 0;
  while ($i < 20) {
  $g = ($g + $x / $g) / 2.0;
  $i = $i + 1;
};
  return $g;
};
  function newRsdv() {
  return ['n' => 0.0, 'a' => 0.0, 'q' => 0.0];
};
  function add($r, $x) {
  $n1 = $r['n'] + 1.0;
  $a1 = $r['a'] + ($x - $r['a']) / $n1;
  $q1 = $r['q'] + ($x - $r['a']) * ($x - $a1);
  return ['n' => $n1, 'a' => $a1, 'q' => $q1];
};
  function sd($r) {
  return sqrtApprox($r['q'] / $r['n']);
};
  function main() {
  $r = newRsdv();
  foreach ([2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0] as $x) {
  $r = add($r, $x);
  echo rtrim(_str(sd($r))), PHP_EOL;
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
