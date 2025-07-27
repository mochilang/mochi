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
  function mochi_xor($a, $b) {
  $res = 0;
  $bit = 1;
  $x = $a;
  $y = $b;
  while ($x > 0 || $y > 0) {
  if ((($x % 2 + $y % 2) % 2 == 1)) {
  $res = $res + $bit;
}
  $x = _intdiv($x, 2);
  $y = _intdiv($y, 2);
  $bit = $bit * 2;
};
  return $res;
};
  function enc($b) {
  return mochi_xor($b, _intdiv($b, 2));
};
  function dec($g) {
  $b = 0;
  $x = $g;
  while ($x > 0) {
  $b = mochi_xor($b, $x);
  $x = _intdiv($x, 2);
};
  return $b;
};
  function binary($n) {
  if ($n == 0) {
  return '0';
}
  $s = '';
  $x = $n;
  while ($x > 0) {
  if ($x % 2 == 1) {
  $s = '1' . $s;
} else {
  $s = '0' . $s;
}
  $x = _intdiv($x, 2);
};
  return $s;
};
  function pad5($s) {
  $p = $s;
  while (strlen($p) < 5) {
  $p = '0' . $p;
};
  return $p;
};
  function main() {
  echo rtrim('decimal  binary   gray    decoded'), PHP_EOL;
  $b = 0;
  while ($b < 32) {
  $g = enc($b);
  $d = dec($g);
  echo rtrim('  ' . pad5(binary($b)) . '   ' . pad5(binary($g)) . '   ' . pad5(binary($d)) . '  ' . _str($d)), PHP_EOL;
  $b = $b + 1;
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
