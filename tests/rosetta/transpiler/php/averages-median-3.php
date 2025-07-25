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
  function qsel($a, $k) {
  $arr = $a;
  while (count($arr) > 1) {
  $px = fmod(_now(), count($arr));
  $pv = $arr[$px];
  $last = count($arr) - 1;
  $tmp = $arr[$px];
  $arr[$px] = $arr[$last];
  $arr[$last] = $tmp;
  $px = 0;
  $i = 0;
  while ($i < $last) {
  $v = $arr[$i];
  if ($v < $pv) {
  $tmp2 = $arr[$px];
  $arr[$px] = $arr[$i];
  $arr[$i] = $tmp2;
  $px = $px + 1;
}
  $i = $i + 1;
};
  if ($px == $k) {
  return $pv;
}
  if ($k < $px) {
  $arr = array_slice($arr, 0, $px - 0);
} else {
  $tmp2 = $arr[$px];
  $arr[$px] = $pv;
  $arr[$last] = $tmp2;
  $arr = array_slice($arr, ($px + 1));
  $k = $k - ($px + 1);
}
};
  return $arr[0];
};
  function median($list) {
  $arr = $list;
  $half = intval((count($arr) / 2));
  $med = qsel($arr, $half);
  if (fmod(count($arr), 2) == 0) {
  return ($med + qsel($arr, $half - 1)) / 2.0;
}
  return $med;
};
  echo rtrim(_str(median([3.0, 1.0, 4.0, 1.0]))), PHP_EOL;
  echo rtrim(_str(median([3.0, 1.0, 4.0, 1.0, 5.0]))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
