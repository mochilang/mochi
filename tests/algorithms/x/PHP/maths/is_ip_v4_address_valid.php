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
$__start_mem = memory_get_usage();
$__start = _now();
  function split_by_dot($s) {
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
  function is_digit_str($s) {
  if (strlen($s) == 0) {
  return false;
}
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if ($c < '0' || $c > '9') {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function parse_decimal($s) {
  $value = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  $value = $value * 10 + (intval($c));
  $i = $i + 1;
};
  return $value;
};
  function is_ip_v4_address_valid($ip) {
  $octets = split_by_dot($ip);
  if (count($octets) != 4) {
  return false;
}
  $i = 0;
  while ($i < 4) {
  $oct = $octets[$i];
  if (!is_digit_str($oct)) {
  return false;
}
  $number = parse_decimal($oct);
  if (strlen(_str($number)) != strlen($oct)) {
  return false;
}
  if ($number < 0 || $number > 255) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  echo rtrim(_str(is_ip_v4_address_valid('192.168.0.23'))), PHP_EOL;
  echo rtrim(_str(is_ip_v4_address_valid('192.256.15.8'))), PHP_EOL;
  echo rtrim(_str(is_ip_v4_address_valid('172.100.0.8'))), PHP_EOL;
  echo rtrim(_str(is_ip_v4_address_valid('255.256.0.256'))), PHP_EOL;
  echo rtrim(_str(is_ip_v4_address_valid('1.2.33333333.4'))), PHP_EOL;
  echo rtrim(_str(is_ip_v4_address_valid('1.2.-3.4'))), PHP_EOL;
  echo rtrim(_str(is_ip_v4_address_valid('1.2.3'))), PHP_EOL;
  echo rtrim(_str(is_ip_v4_address_valid('1.2.3.4.5'))), PHP_EOL;
  echo rtrim(_str(is_ip_v4_address_valid('1.2.A.4'))), PHP_EOL;
  echo rtrim(_str(is_ip_v4_address_valid('0.0.0.0'))), PHP_EOL;
  echo rtrim(_str(is_ip_v4_address_valid('1.2.3.'))), PHP_EOL;
  echo rtrim(_str(is_ip_v4_address_valid('1.2.3.05'))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
