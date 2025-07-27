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
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
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
  function floorf($x) {
  $y = intval($x);
  return floatval($y);
};
  function indexOf($s, $ch) {
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return 0 - 1;
};
  function fmt8($x) {
  $y = floorf($x * 100000000.0 + 0.5) / 100000000.0;
  $s = _str($y);
  $dot = _indexof($s, '.');
  if ($dot == 0 - 1) {
  $s = $s . '.00000000';
} else {
  $decs = strlen($s) - $dot - 1;
  while ($decs < 8) {
  $s = $s . '0';
  $decs = $decs + 1;
};
}
  return $s;
};
  function pad2($x) {
  $s = _str($x);
  if (strlen($s) < 2) {
  $s = ' ' . $s;
}
  return $s;
};
  function main() {
  $maxIt = 13;
  $maxItJ = 10;
  $a1 = 1.0;
  $a2 = 0.0;
  $d1 = 3.2;
  echo rtrim(' i       d'), PHP_EOL;
  $i = 2;
  while ($i <= $maxIt) {
  $a = $a1 + ($a1 - $a2) / $d1;
  $j = 1;
  while ($j <= $maxItJ) {
  $x = 0.0;
  $y = 0.0;
  $k = 1;
  $limit = pow_int(2, $i);
  while ($k <= $limit) {
  $y = 1.0 - 2.0 * $y * $x;
  $x = $a - $x * $x;
  $k = $k + 1;
};
  $a = $a - $x / $y;
  $j = $j + 1;
};
  $d = ($a1 - $a2) / ($a - $a1);
  echo rtrim(pad2($i) . '    ' . fmt8($d)), PHP_EOL;
  $d1 = $d;
  $a2 = $a1;
  $a1 = $a;
  $i = $i + 1;
};
};
  function pow_int($base, $exp) {
  $r = 1;
  $b = $base;
  $e = $exp;
  while ($e > 0) {
  if ($e % 2 == 1) {
  $r = $r * $b;
}
  $b = $b * $b;
  $e = intval((_intdiv($e, 2)));
};
  return $r;
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
