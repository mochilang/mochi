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
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function egcd($a, $b) {
  global $n;
  if ($a == 0) {
  return [$b, 0, 1];
}
  $res = egcd($b % $a, $a);
  $g = $res[0];
  $x1 = $res[1];
  $y1 = $res[2];
  return [$g, $y1 - (_intdiv($b, $a)) * $x1, $x1];
};
  function modInv($a, $m) {
  global $n, $res;
  $r = egcd($a, $m);
  if ($r[0] != 1) {
  return 0;
}
  $x = $r[1];
  if ($x < 0) {
  return $x + $m;
}
  return $x;
};
  function crt($a, $n) {
  global $res;
  $prod = 1;
  $i = 0;
  while ($i < count($n)) {
  $prod = $prod * $n[$i];
  $i = $i + 1;
};
  $x = 0;
  $i = 0;
  while ($i < count($n)) {
  $ni = $n[$i];
  $ai = $a[$i];
  $p = _intdiv($prod, $ni);
  $inv = modInv($p % $ni, $ni);
  $x = $x + $ai * $inv * $p;
  $i = $i + 1;
};
  return $x % $prod;
};
  $n = [3, 5, 7];
  $a = [2, 3, 2];
  $res = crt($a, $n);
  echo rtrim(_str($res) . ' <nil>'), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
