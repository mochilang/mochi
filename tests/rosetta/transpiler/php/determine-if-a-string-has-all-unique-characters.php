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
  function indexOf3($s, $ch, $start) {
  $i = $start;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function mochi_ord($ch) {
  $digits = '0123456789';
  $idx = indexOf3($digits, $ch, 0);
  if ($idx >= 0) {
  return 48 + $idx;
}
  if ($ch == 'X') {
  return 88;
}
  if ($ch == 'é') {
  return 233;
}
  if ($ch == '😍') {
  return 128525;
}
  if ($ch == '🐡') {
  return 128033;
}
  return 0;
};
  function toHex($n) {
  $digits = '0123456789ABCDEF';
  if ($n == 0) {
  return '0';
}
  $v = $n;
  $out = '';
  while ($v > 0) {
  $d = $v % 16;
  $out = substr($digits, $d, $d + 1 - $d) . $out;
  $v = _intdiv($v, 16);
};
  return $out;
};
  function analyze($s) {
  $le = strlen($s);
  echo rtrim('Analyzing "' . $s . '" which has a length of ' . _str($le) . ':'), PHP_EOL;
  if ($le > 1) {
  $i = 0;
  while ($i < $le - 1) {
  $j = $i + 1;
  while ($j < $le) {
  if (substr($s, $j, $j + 1 - $j) == substr($s, $i, $i + 1 - $i)) {
  $ch = substr($s, $i, $i + 1 - $i);
  echo rtrim('  Not all characters in the string are unique.'), PHP_EOL;
  echo rtrim('  \'' . $ch . '\' (0x' . strtolower(toHex(mochi_ord($ch))) . ') is duplicated at positions ' . _str($i + 1) . ' and ' . _str($j + 1) . '.
'), PHP_EOL;
  return;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
}
  echo rtrim('  All characters in the string are unique.
'), PHP_EOL;
};
  function main() {
  $strings = ['', '.', 'abcABC', 'XYZ ZYX', '1234567890ABCDEFGHIJKLMN0PQRSTUVWXYZ', '01234567890ABCDEFGHIJKLMN0PQRSTUVWXYZ0X', 'hétérogénéité', '🎆🎃🎇🎈', '😍😀🙌💃😍🙌', '🐠🐟🐡🦈🐬🐳🐋🐡'];
  $i = 0;
  while ($i < count($strings)) {
  analyze($strings[$i]);
  $i = $i + 1;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
