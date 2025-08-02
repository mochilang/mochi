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
  $n = 64;
  function pow2($k) {
  global $n, $init, $bytes;
  $v = 1;
  $i = 0;
  while ($i < $k) {
  $v = $v * 2;
  $i = $i + 1;
};
  return $v;
};
  function ruleBit($ruleNum, $idx) {
  global $n, $init, $bytes;
  $r = $ruleNum;
  $i = 0;
  while ($i < $idx) {
  $r = _intdiv($r, 2);
  $i = $i + 1;
};
  return $r % 2;
};
  function evolve($state, $ruleNum) {
  global $n, $init, $bytes;
  $out = [];
  $p = 0;
  while ($p < 10) {
  $b = 0;
  $q = 7;
  while ($q >= 0) {
  $st = $state;
  $b = $b + $st[0] * pow2($q);
  $next = [];
  $i = 0;
  while ($i < $n) {
  $lidx = $i - 1;
  if ($lidx < 0) {
  $lidx = $n - 1;
}
  $left = $st[$lidx];
  $center = $st[$i];
  $ridx = $i + 1;
  if ($ridx >= $n) {
  $ridx = 0;
}
  $right = $st[$ridx];
  $index = $left * 4 + $center * 2 + $right;
  $next = array_merge($next, [ruleBit($ruleNum, $index)]);
  $i = $i + 1;
};
  $state = $next;
  $q = $q - 1;
};
  $out = array_merge($out, [$b]);
  $p = $p + 1;
};
  return $out;
};
  $init = [];
  $i = 0;
  while ($i < $n) {
  $init = array_merge($init, [0]);
  $i = $i + 1;
}
  $init[0] = 1;
  $bytes = evolve($init, 30);
  echo rtrim(_str($bytes)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
