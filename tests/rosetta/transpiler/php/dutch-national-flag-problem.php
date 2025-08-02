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
  function listStr($xs) {
  $s = '[';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . _str($xs[$i]);
  if ($i < count($xs) - 1) {
  $s = $s . ' ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
};
  function ordered($xs) {
  if (count($xs) == 0) {
  return true;
}
  $prev = $xs[0];
  $i = 1;
  while ($i < count($xs)) {
  if ($xs[$i] < $prev) {
  return false;
}
  $prev = $xs[$i];
  $i = $i + 1;
};
  return true;
};
  function outOfOrder($n) {
  if ($n < 2) {
  return [];
}
  $r = [];
  while (true) {
  $r = [];
  $i = 0;
  while ($i < $n) {
  $r = array_merge($r, [fmod(_now(), 3)]);
  $i = $i + 1;
};
  if (!ordered($r)) {
  break;
}
};
  return $r;
};
  function sort3(&$a) {
  $lo = 0;
  $mid = 0;
  $hi = count($a) - 1;
  while ($mid <= $hi) {
  $v = $a[$mid];
  if ($v == 0) {
  $tmp = $a[$lo];
  $a[$lo] = $a[$mid];
  $a[$mid] = $tmp;
  $lo = $lo + 1;
  $mid = $mid + 1;
} else {
  if ($v == 1) {
  $mid = $mid + 1;
} else {
  $tmp = $a[$mid];
  $a[$mid] = $a[$hi];
  $a[$hi] = $tmp;
  $hi = $hi - 1;
};
}
};
  return $a;
};
  function main() {
  $f = outOfOrder(12);
  echo rtrim(listStr($f)), PHP_EOL;
  $f = sort3($f);
  echo rtrim(listStr($f)), PHP_EOL;
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
