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
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  $ascii = ' !"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~';
  function mochi_ord($ch) {
  global $ascii;
  $i = 0;
  while ($i < strlen($ascii)) {
  if (substr($ascii, $i, $i + 1 - $i) == $ch) {
  return 32 + $i;
}
  $i = $i + 1;
};
  return 0;
};
  function bit_and($a, $b) {
  global $ascii;
  $ua = $a;
  $ub = $b;
  $res = 0;
  $bit = 1;
  while ($ua > 0 || $ub > 0) {
  if ($ua % 2 == 1 && $ub % 2 == 1) {
  $res = $res + $bit;
}
  $ua = intval((_intdiv($ua, 2)));
  $ub = intval((_intdiv($ub, 2)));
  $bit = $bit * 2;
};
  return $res;
};
  function bit_xor($a, $b) {
  global $ascii;
  $ua = $a;
  $ub = $b;
  $res = 0;
  $bit = 1;
  while ($ua > 0 || $ub > 0) {
  $abit = $ua % 2;
  $bbit = $ub % 2;
  if ($abit != $bbit) {
  $res = $res + $bit;
}
  $ua = intval((_intdiv($ua, 2)));
  $ub = intval((_intdiv($ub, 2)));
  $bit = $bit * 2;
};
  return $res;
};
  function bit_not32($x) {
  global $ascii;
  $ux = $x;
  $res = 0;
  $bit = 1;
  $count = 0;
  while ($count < 32) {
  if ($ux % 2 == 0) {
  $res = $res + $bit;
}
  $ux = intval((_intdiv($ux, 2)));
  $bit = $bit * 2;
  $count = $count + 1;
};
  return $res;
};
  function elf_hash($data) {
  global $ascii;
  $hash_ = 0;
  $i = 0;
  while ($i < strlen($data)) {
  $c = mochi_ord(substr($data, $i, $i + 1 - $i));
  $hash_ = $hash_ * 16 + $c;
  $x = bit_and($hash_, 4026531840);
  if ($x != 0) {
  $hash_ = bit_xor($hash_, intval((_intdiv($x, 16777216))));
}
  $hash_ = bit_and($hash_, bit_not32($x));
  $i = $i + 1;
};
  return $hash_;
};
  echo rtrim(_str(elf_hash('lorem ipsum'))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
