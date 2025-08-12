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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function contains($xs, $value) {
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $value) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function solution($n) {
  $zmulti = [];
  $xmulti = [];
  $temp = 1;
  while (true) {
  $result = 3 * $temp;
  if ($result < $n) {
  $zmulti = _append($zmulti, $result);
  $temp = $temp + 1;
} else {
  break;
}
};
  $temp = 1;
  while (true) {
  $result = 5 * $temp;
  if ($result < $n) {
  $xmulti = _append($xmulti, $result);
  $temp = $temp + 1;
} else {
  break;
}
};
  $collection = [];
  $i = 0;
  while ($i < count($zmulti)) {
  $v = $zmulti[$i];
  if (!contains($collection, $v)) {
  $collection = _append($collection, $v);
}
  $i = $i + 1;
};
  $i = 0;
  while ($i < count($xmulti)) {
  $v = $xmulti[$i];
  if (!contains($collection, $v)) {
  $collection = _append($collection, $v);
}
  $i = $i + 1;
};
  $total = 0;
  $i = 0;
  while ($i < count($collection)) {
  $total = $total + $collection[$i];
  $i = $i + 1;
};
  return $total;
};
  function test_solution() {
  if (solution(3) != 0) {
  _panic('solution(3) failed');
}
  if (solution(4) != 3) {
  _panic('solution(4) failed');
}
  if (solution(10) != 23) {
  _panic('solution(10) failed');
}
  if (solution(600) != 83700) {
  _panic('solution(600) failed');
}
};
  function main() {
  test_solution();
  echo rtrim('solution() = ' . _str(solution(1000))), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
