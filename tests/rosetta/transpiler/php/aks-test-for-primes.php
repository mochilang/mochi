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
  function poly($p) {
  $s = '';
  $coef = 1;
  $i = $p;
  if ($coef != 1) {
  $s = $s . _str($coef);
}
  while ($i > 0) {
  $s = $s . 'x';
  if ($i != 1) {
  $s = $s . '^' . _str($i);
}
  $coef = intval((_intdiv($coef * $i, ($p - $i + 1))));
  $d = $coef;
  if (($p - ($i - 1)) % 2 == 1) {
  $d = -$d;
}
  if ($d < 0) {
  $s = $s . ' - ' . _str(-$d);
} else {
  $s = $s . ' + ' . _str($d);
}
  $i = $i - 1;
};
  if ($s == '') {
  $s = '1';
}
  return $s;
};
  function aks($n) {
  if ($n < 2) {
  return false;
}
  $c = $n;
  $i = 1;
  while ($i < $n) {
  if ($c % $n != 0) {
  return false;
}
  $c = intval((_intdiv($c * ($n - $i), ($i + 1))));
  $i = $i + 1;
};
  return true;
};
  function main() {
  $p = 0;
  while ($p <= 7) {
  echo rtrim(_str($p) . ':  ' . poly($p)), PHP_EOL;
  $p = $p + 1;
};
  $first = true;
  $p = 2;
  $line = '';
  while ($p < 50) {
  if (aks($p)) {
  if ($first) {
  $line = $line . _str($p);
  $first = false;
} else {
  $line = $line . ' ' . _str($p);
};
}
  $p = $p + 1;
};
  echo rtrim($line), PHP_EOL;
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
