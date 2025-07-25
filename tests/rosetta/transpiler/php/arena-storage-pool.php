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
  function poolPut($p, $x) {
  return array_merge($p, [$x]);
};
  function poolGet($p) {
  if (count($p) == 0) {
  echo rtrim('pool empty'), PHP_EOL;
  return ['pool' => $p, 'val' => 0];
}
  $idx = count($p) - 1;
  $v = $p[$idx];
  $p = array_slice($p, 0, $idx - 0);
  return ['pool' => $p, 'val' => $v];
};
  function clearPool($p) {
  return [];
};
  function main() {
  $pool = [];
  $i = 1;
  $j = 2;
  echo rtrim(_str($i + $j)), PHP_EOL;
  $pool = poolPut($pool, $i);
  $pool = poolPut($pool, $j);
  $i = 0;
  $j = 0;
  $res1 = poolGet($pool);
  $pool = $res1['pool'];
  $i = ord($res1['val']);
  $res2 = poolGet($pool);
  $pool = $res2['pool'];
  $j = ord($res2['val']);
  $i = 4;
  $j = 5;
  echo rtrim(_str($i + $j)), PHP_EOL;
  $pool = poolPut($pool, $i);
  $pool = poolPut($pool, $j);
  $i = 0;
  $j = 0;
  $pool = clearPool($pool);
  $res3 = poolGet($pool);
  $pool = $res3['pool'];
  $i = ord($res3['val']);
  $res4 = poolGet($pool);
  $pool = $res4['pool'];
  $j = ord($res4['val']);
  $i = 7;
  $j = 8;
  echo rtrim(_str($i + $j)), PHP_EOL;
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
