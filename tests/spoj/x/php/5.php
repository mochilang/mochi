<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
function parseIntStr($s, $base = 10) {
    return intval($s, intval($base));
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        $q = bcdiv($sa, $sb, 0);
        $rem = bcmod($sa, $sb);
        $neg = ((strpos($sa, '-') === 0) xor (strpos($sb, '-') === 0));
        if ($neg && bccomp($rem, '0') != 0) {
            $q = bcsub($q, '1');
        }
        return intval($q);
    }
    $ai = intval($a);
    $bi = intval($b);
    $q = intdiv($ai, $bi);
    if ((($ai ^ $bi) < 0) && ($ai % $bi != 0)) {
        $q -= 1;
    }
    return $q;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function next_pal($s) {
  $digitMap = ['0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9];
  $n = strlen($s);
  $num = [];
  for ($i = 0; $i < $n; $i++) {
  $num = _append($num, intval($digitMap[$s[$i]]));
};
  $all9 = true;
  foreach ($num as $d) {
  if ($d != 9) {
  $all9 = false;
  break;
}
};
  if ($all9) {
  $res = '1';
  for ($_ = 0; $_ < ($n - 1); $_++) {
  $res = $res . '0';
};
  $res = $res . '1';
  return $res;
}
  $left = _intdiv($n, 2) - 1;
  $right = ($n % 2 == 0 ? _intdiv($n, 2) : _intdiv($n, 2) + 1);
  while ($left >= 0 && $right < $n && $num[$left] == $num[$right]) {
  $left = $left - 1;
  $right = $right + 1;
};
  $smaller = $left < 0 || $num[$left] < $num[$right];
  $left = _intdiv($n, 2) - 1;
  $right = ($n % 2 == 0 ? _intdiv($n, 2) : _intdiv($n, 2) + 1);
  while ($left >= 0) {
  $num[$right] = $num[$left];
  $left = $left - 1;
  $right = $right + 1;
};
  if ($smaller) {
  $carry = 1;
  $left = _intdiv($n, 2) - 1;
  if ($n % 2 == 1) {
  $mid = _intdiv($n, 2);
  $num[$mid] = $num[$mid] + $carry;
  $carry = $num[$mid] / 10;
  $num[$mid] = fmod($num[$mid], 10);
  $right = $mid + 1;
} else {
  $right = _intdiv($n, 2);
};
  while ($left >= 0) {
  $num[$left] = $num[$left] + $carry;
  $carry = $num[$left] / 10;
  $num[$left] = fmod($num[$left], 10);
  $num[$right] = $num[$left];
  $left = $left - 1;
  $right = $right + 1;
};
}
  $out = '';
  foreach ($num as $d) {
  $out = $out . _str($d);
};
  return $out;
};
  function mochi_parseIntStr($str) {
  $digits = ['0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9];
  $i = 0;
  $n = 0;
  while ($i < strlen($str)) {
  $n = $n * 10 + (intval($digits[$str[$i]]));
  $i = $i + 1;
};
  return $n;
};
  function main() {
  $tStr = trim(fgets(STDIN));
  if ($tStr == '') {
  return;
}
  $t = parseIntStr($tStr, 10);
  for ($_ = 0; $_ < $t; $_++) {
  $s = trim(fgets(STDIN));
  echo rtrim(next_pal($s)), PHP_EOL;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
