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
function repeat($s, $n) {
    return str_repeat($s, intval($n));
}
function parseIntStr($s, $base = 10) {
    return intval($s, intval($base));
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
  function split($s, $sep) {
  global $tests;
  $parts = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($s)) {
  if (strlen($sep) > 0 && $i + strlen($sep) <= strlen($s) && substr($s, $i, $i + strlen($sep) - $i) == $sep) {
  $parts = array_merge($parts, [$cur]);
  $cur = '';
  $i = $i + strlen($sep);
} else {
  $cur = $cur . substr($s, $i, $i + 1 - $i);
  $i = $i + 1;
}
};
  $parts = array_merge($parts, [$cur]);
  return $parts;
};
  function mochi_join($xs, $sep) {
  global $tests;
  $res = '';
  $i = 0;
  while ($i < count($xs)) {
  if ($i > 0) {
  $res = $res . $sep;
}
  $res = $res . $xs[$i];
  $i = $i + 1;
};
  return $res;
};
  function mochi_repeat($ch, $n) {
  global $tests;
  $out = '';
  $i = 0;
  while ($i < $n) {
  $out = $out . $ch;
  $i = $i + 1;
};
  return $out;
};
  function mochi_parseIntStr($str) {
  global $tests;
  $i = 0;
  $neg = false;
  if (strlen($str) > 0 && substr($str, 0, 1 - 0) == '-') {
  $neg = true;
  $i = 1;
}
  $n = 0;
  $digits = ['0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9];
  while ($i < strlen($str)) {
  $n = $n * 10 + $digits[substr($str, $i, $i + 1 - $i)];
  $i = $i + 1;
};
  if ($neg) {
  $n = -$n;
}
  return $n;
};
  function toBinary($n, $bits) {
  global $tests;
  $b = '';
  $val = $n;
  $i = 0;
  while ($i < $bits) {
  $b = _str($val % 2) . $b;
  $val = intval((_intdiv($val, 2)));
  $i = $i + 1;
};
  return $b;
};
  function binToInt($bits) {
  global $tests;
  $n = 0;
  $i = 0;
  while ($i < strlen($bits)) {
  $n = $n * 2 + parseIntStr(substr($bits, $i, $i + 1 - $i), 10);
  $i = $i + 1;
};
  return $n;
};
  function padRight($s, $width) {
  global $tests;
  $out = $s;
  while (strlen($out) < $width) {
  $out = $out . ' ';
};
  return $out;
};
  function canonicalize($cidr) {
  global $tests;
  $parts = explode('/', $cidr);
  $dotted = $parts[0];
  $size = parseIntStr($parts[1], 10);
  $binParts = [];
  foreach (explode('.', $dotted) as $p) {
  $binParts = array_merge($binParts, [toBinary(parseIntStr($p, 10), 8)]);
};
  $binary = mochi_join($binParts, '');
  $binary = substr($binary, 0, $size - 0) . repeat('0', 32 - $size);
  $canonParts = [];
  $i = 0;
  while ($i < strlen($binary)) {
  $canonParts = array_merge($canonParts, [_str(binToInt(substr($binary, $i, $i + 8 - $i)))]);
  $i = $i + 8;
};
  return mochi_join($canonParts, '.') . '/' . $parts[1];
};
  $tests = ['87.70.141.1/22', '36.18.154.103/12', '62.62.197.11/29', '67.137.119.181/4', '161.214.74.21/24', '184.232.176.184/18'];
  foreach ($tests as $t) {
  echo rtrim(padRight($t, 18) . ' -> ' . canonicalize($t)), PHP_EOL;
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
