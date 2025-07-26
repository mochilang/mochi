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
  function gcd($a, $b) {
  $x = $a;
  if ($x < 0) {
  $x = -$x;
}
  $y = $b;
  if ($y < 0) {
  $y = -$y;
}
  while ($y != 0) {
  $t = $x % $y;
  $x = $y;
  $y = $t;
};
  return $x;
};
  function parseRational($s) {
  $intPart = 0;
  $fracPart = 0;
  $denom = 1;
  $afterDot = false;
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == '.') {
  $afterDot = true;
} else {
  $d = intval($ch) - intval('0');
  if (!$afterDot) {
  $intPart = $intPart * 10 + $d;
} else {
  $fracPart = $fracPart * 10 + $d;
  $denom = $denom * 10;
};
}
  $i = $i + 1;
};
  $num = $intPart * $denom + $fracPart;
  $g = gcd($num, $denom);
  return ['num' => intval((_intdiv($num, $g))), 'den' => intval((_intdiv($denom, $g)))];
};
  function main() {
  $inputs = ['0.9054054', '0.518518', '0.75'];
  foreach ($inputs as $s) {
  $r = parseRational($s);
  echo rtrim($s . ' = ' . _str($r['num']) . '/' . _str($r['den'])), PHP_EOL;
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
