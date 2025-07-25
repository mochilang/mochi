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
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function _sha256($bs) {
    $bin = '';
    foreach ($bs as $b) { $bin .= chr($b); }
    $hash = hash('sha256', $bin, true);
    return array_values(unpack('C*', $hash));
}
$__start_mem = memory_get_usage();
$__start = _now();
  function indexOf($s, $ch) {
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function set58($addr) {
  $tmpl = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz';
  $a = [];
  $i = 0;
  while ($i < 25) {
  $a = array_merge($a, [0]);
  $i = $i + 1;
};
  $idx = 0;
  while ($idx < strlen($addr)) {
  $ch = substr($addr, $idx, $idx + 1 - $idx);
  $c = _indexof($tmpl, $ch);
  if ($c < 0) {
  return [];
}
  $j = 24;
  while ($j >= 0) {
  $c = $c + 58 * $a[$j];
  $a[$j] = $c % 256;
  $c = intval((_intdiv($c, 256)));
  $j = $j - 1;
};
  if ($c > 0) {
  return [];
}
  $idx = $idx + 1;
};
  return $a;
};
  function doubleSHA256($bs) {
  $first = _sha256($bs);
  return _sha256($first);
};
  function computeChecksum($a) {
  $hash = doubleSHA256(array_slice($a, 0, 21 - 0));
  return array_slice($hash, 0, 4 - 0);
};
  function validA58($addr) {
  $a = set58($addr);
  if (count($a) != 25) {
  return false;
}
  if ($a[0] != 0) {
  return false;
}
  $sum = computeChecksum($a);
  $i = 0;
  while ($i < 4) {
  if ($a[21 + $i] != $sum[$i]) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  echo rtrim(_str(validA58('1AGNa15ZQXAZUgFiqJ3i7Z2DPU2J6hW62i'))), PHP_EOL;
  echo rtrim(_str(validA58('17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j'))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
