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
  function randPerm($n) {
  $arr = [];
  $i = 0;
  while ($i < $n) {
  $arr = array_merge($arr, [$i]);
  $i = $i + 1;
};
  $idx = $n - 1;
  while ($idx > 0) {
  $j = fmod(_now(), ($idx + 1));
  $tmp = $arr[$idx];
  $arr[$idx] = $arr[$j];
  $arr[$j] = $tmp;
  $idx = $idx - 1;
};
  return $arr;
};
  function even($xs) {
  $r = [];
  foreach ($xs as $x) {
  if ($x % 2 == 0) {
  $r = array_merge($r, [$x]);
}
};
  return $r;
};
  function reduceToEven($xs) {
  $arr = $xs;
  $last = 0;
  $i = 0;
  while ($i < count($arr)) {
  $e = $arr[$i];
  if ($e % 2 == 0) {
  $arr[$last] = $e;
  $last = $last + 1;
}
  $i = $i + 1;
};
  return array_slice($arr, 0, $last - 0);
};
  function listStr($xs) {
  $s = '[';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . _str($xs[$i]);
  if ($i + 1 < count($xs)) {
  $s = $s . ' ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
};
  function main() {
  $a = randPerm(20);
  $cap_a = 20;
  echo rtrim(listStr($a)), PHP_EOL;
  echo rtrim(listStr(even($a))), PHP_EOL;
  echo rtrim(listStr($a)), PHP_EOL;
  $a = reduceToEven($a);
  echo rtrim(listStr($a)), PHP_EOL;
  echo rtrim('a len: ' . _str(count($a)) . ' cap: ' . _str($cap_a)), PHP_EOL;
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
