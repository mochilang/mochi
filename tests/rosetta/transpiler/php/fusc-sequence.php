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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function fuscVal($n) {
  $a = 1;
  $b = 0;
  $x = $n;
  while ($x > 0) {
  if ($x % 2 == 0) {
  $x = _intdiv($x, 2);
  $a = $a + $b;
} else {
  $x = _intdiv(($x - 1), 2);
  $b = $a + $b;
}
};
  if ($n == 0) {
  return 0;
}
  return $b;
};
  function firstFusc($n) {
  $arr = [];
  $i = 0;
  while ($i < $n) {
  $arr = array_merge($arr, [fuscVal($i)]);
  $i = $i + 1;
};
  return $arr;
};
  function commatize($n) {
  $s = _str($n);
  $neg = false;
  if ($n < 0) {
  $neg = true;
  $s = substr($s, 1, strlen($s) - 1);
}
  $i = strlen($s) - 3;
  while ($i >= 1) {
  $s = substr($s, 0, $i - 0) . ',' . substr($s, $i, strlen($s) - $i);
  $i = $i - 3;
};
  if ($neg) {
  return '-' . $s;
}
  return $s;
};
  function padLeft($s, $w) {
  $out = $s;
  while (strlen($out) < $w) {
  $out = ' ' . $out;
};
  return $out;
};
  function main() {
  echo rtrim('The first 61 fusc numbers are:'), PHP_EOL;
  echo rtrim(_str(firstFusc(61))), PHP_EOL;
  echo rtrim('
The fusc numbers whose length > any previous fusc number length are:'), PHP_EOL;
  $idxs = [0, 37, 1173, 35499, 699051, 19573419];
  $i = 0;
  while ($i < count($idxs)) {
  $idx = $idxs[$i];
  $val = fuscVal($idx);
  $numStr = padLeft(commatize($val), 7);
  $idxStr = padLeft(commatize($idx), 10);
  echo rtrim($numStr . ' (index ' . $idxStr . ')'), PHP_EOL;
  $i = $i + 1;
};
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
