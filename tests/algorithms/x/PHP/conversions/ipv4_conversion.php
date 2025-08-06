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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
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
  $hex_digits = '0123456789abcdef';
  function split_by_dot($s) {
  global $hex_digits;
  $res = [];
  $current = '';
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if ($c == '.') {
  $res = _append($res, $current);
  $current = '';
} else {
  $current = $current . $c;
}
  $i = $i + 1;
};
  $res = _append($res, $current);
  return $res;
};
  function parse_decimal($s) {
  global $hex_digits;
  if (strlen($s) == 0) {
  $panic('Invalid IPv4 address format');
}
  $value = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if ($c < '0' || $c > '9') {
  $panic('Invalid IPv4 address format');
}
  $value = $value * 10 + (intval($c));
  $i = $i + 1;
};
  return $value;
};
  function to_hex2($n) {
  global $hex_digits;
  $x = $n;
  $res = '';
  while ($x > 0) {
  $d = $x % 16;
  $res = substr($hex_digits, $d, $d + 1 - $d) . $res;
  $x = _intdiv($x, 16);
};
  while (strlen($res) < 2) {
  $res = '0' . $res;
};
  return $res;
};
  function ipv4_to_decimal($ipv4_address) {
  global $hex_digits;
  $parts = split_by_dot($ipv4_address);
  if (count($parts) != 4) {
  $panic('Invalid IPv4 address format');
}
  $result = 0;
  $i = 0;
  while ($i < 4) {
  $oct = parse_decimal($parts[$i]);
  if ($oct < 0 || $oct > 255) {
  $panic('Invalid IPv4 octet ' . _str($oct));
}
  $result = $result * 256 + $oct;
  $i = $i + 1;
};
  return $result;
};
  function alt_ipv4_to_decimal($ipv4_address) {
  global $hex_digits;
  $parts = split_by_dot($ipv4_address);
  if (count($parts) != 4) {
  $panic('Invalid IPv4 address format');
}
  $hex_str = '';
  $i = 0;
  while ($i < 4) {
  $oct = parse_decimal($parts[$i]);
  if ($oct < 0 || $oct > 255) {
  $panic('Invalid IPv4 octet ' . _str($oct));
}
  $hex_str = $hex_str . to_hex2($oct);
  $i = $i + 1;
};
  $value = 0;
  $k = 0;
  while ($k < strlen($hex_str)) {
  $c = substr($hex_str, $k, $k + 1 - $k);
  $digit = 0 - 1;
  $j = 0;
  while ($j < strlen($hex_digits)) {
  if (substr($hex_digits, $j, $j + 1 - $j) == $c) {
  $digit = $j;
}
  $j = $j + 1;
};
  if ($digit < 0) {
  $panic('Invalid hex digit');
}
  $value = $value * 16 + $digit;
  $k = $k + 1;
};
  return $value;
};
  function decimal_to_ipv4($decimal_ipv4) {
  global $hex_digits;
  if ($decimal_ipv4 < 0 || $decimal_ipv4 > 4294967295) {
  $panic('Invalid decimal IPv4 address');
}
  $n = $decimal_ipv4;
  $parts = [];
  $i = 0;
  while ($i < 4) {
  $octet = $n % 256;
  $parts = _append($parts, _str($octet));
  $n = _intdiv($n, 256);
  $i = $i + 1;
};
  $res = '';
  $j = count($parts) - 1;
  while ($j >= 0) {
  $res = $res . $parts[$j];
  if ($j > 0) {
  $res = $res . '.';
}
  $j = $j - 1;
};
  return $res;
};
  echo rtrim(json_encode(ipv4_to_decimal('192.168.0.1'), 1344)), PHP_EOL;
  echo rtrim(json_encode(ipv4_to_decimal('10.0.0.255'), 1344)), PHP_EOL;
  echo rtrim(json_encode(alt_ipv4_to_decimal('192.168.0.1'), 1344)), PHP_EOL;
  echo rtrim(json_encode(alt_ipv4_to_decimal('10.0.0.255'), 1344)), PHP_EOL;
  echo rtrim(decimal_to_ipv4(3232235521)), PHP_EOL;
  echo rtrim(decimal_to_ipv4(167772415)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
