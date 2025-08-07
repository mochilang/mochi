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
  function apply_table($inp, $table) {
  global $p4_table, $key, $message, $p8_table, $p10_table, $IP, $IP_inv, $expansion, $s0, $s1, $temp, $left, $right, $key1, $key2, $CT, $PT;
  $res = '';
  $i = 0;
  while ($i < count($table)) {
  $idx = $table[$i] - 1;
  if ($idx < 0) {
  $idx = strlen($inp) - 1;
}
  $res = $res . substr($inp, $idx, $idx + 1 - $idx);
  $i = $i + 1;
};
  return $res;
};
  function left_shift($data) {
  global $p4_table, $key, $message, $p8_table, $p10_table, $IP, $IP_inv, $expansion, $s0, $s1, $temp, $left, $right, $key1, $key2, $CT, $PT;
  return substr($data, 1, strlen($data) - 1) . substr($data, 0, 1 - 0);
};
  function mochi_xor($a, $b) {
  global $p4_table, $key, $message, $p8_table, $p10_table, $IP, $IP_inv, $expansion, $s0, $s1, $temp, $left, $right, $key1, $key2, $CT, $PT;
  $res = '';
  $i = 0;
  while ($i < strlen($a) && $i < strlen($b)) {
  if (substr($a, $i, $i + 1 - $i) == substr($b, $i, $i + 1 - $i)) {
  $res = $res . '0';
} else {
  $res = $res . '1';
}
  $i = $i + 1;
};
  return $res;
};
  function int_to_binary($n) {
  global $p4_table, $key, $message, $p8_table, $p10_table, $IP, $IP_inv, $expansion, $s0, $s1, $temp, $left, $right, $key1, $key2, $CT, $PT;
  if ($n == 0) {
  return '0';
}
  $res = '';
  $num = $n;
  while ($num > 0) {
  $res = _str($num % 2) . $res;
  $num = _intdiv($num, 2);
};
  return $res;
};
  function pad_left($s, $width) {
  global $p4_table, $key, $message, $p8_table, $p10_table, $IP, $IP_inv, $expansion, $s0, $s1, $temp, $left, $right, $key1, $key2, $CT, $PT;
  $res = $s;
  while (strlen($res) < $width) {
  $res = '0' . $res;
};
  return $res;
};
  function bin_to_int($s) {
  global $p4_table, $key, $message, $p8_table, $p10_table, $IP, $IP_inv, $expansion, $s0, $s1, $temp, $left, $right, $key1, $key2, $CT, $PT;
  $result = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $digit = ord(substr($s, $i, $i + 1 - $i));
  $result = $result * 2 + $digit;
  $i = $i + 1;
};
  return $result;
};
  function apply_sbox($s, $data) {
  global $p4_table, $key, $message, $p8_table, $p10_table, $IP, $IP_inv, $expansion, $s0, $s1, $temp, $left, $right, $key1, $key2, $CT, $PT;
  $row_bits = substr($data, 0, 1 - 0) . substr($data, strlen($data) - 1, strlen($data) - (strlen($data) - 1));
  $col_bits = substr($data, 1, 3 - 1);
  $row = bin_to_int($row_bits);
  $col = bin_to_int($col_bits);
  $val = $s[$row][$col];
  $out = int_to_binary($val);
  return $out;
};
  $p4_table = [2, 4, 3, 1];
  function f($expansion, $s0, $s1, $key, $message) {
  global $p4_table, $p8_table, $p10_table, $IP, $IP_inv, $key1, $key2, $CT, $PT;
  $left = substr($message, 0, 4 - 0);
  $right = substr($message, 4, 8 - 4);
  $temp = apply_table($right, $expansion);
  $temp = mochi_xor($temp, $key);
  $left_bin_str = apply_sbox($s0, substr($temp, 0, 4 - 0));
  $right_bin_str = apply_sbox($s1, substr($temp, 4, 8 - 4));
  $left_bin_str = pad_left($left_bin_str, 2);
  $right_bin_str = pad_left($right_bin_str, 2);
  $temp = apply_table($left_bin_str . $right_bin_str, $p4_table);
  $temp = mochi_xor($left, $temp);
  return $temp . $right;
};
  $key = '1010000010';
  $message = '11010111';
  $p8_table = [6, 3, 7, 4, 8, 5, 10, 9];
  $p10_table = [3, 5, 2, 7, 4, 10, 1, 9, 8, 6];
  $IP = [2, 6, 3, 1, 4, 8, 5, 7];
  $IP_inv = [4, 1, 3, 5, 7, 2, 8, 6];
  $expansion = [4, 1, 2, 3, 2, 3, 4, 1];
  $s0 = [[1, 0, 3, 2], [3, 2, 1, 0], [0, 2, 1, 3], [3, 1, 3, 2]];
  $s1 = [[0, 1, 2, 3], [2, 0, 1, 3], [3, 0, 1, 0], [2, 1, 0, 3]];
  $temp = apply_table($key, $p10_table);
  $left = substr($temp, 0, 5 - 0);
  $right = substr($temp, 5, 10 - 5);
  $left = left_shift($left);
  $right = left_shift($right);
  $key1 = apply_table($left . $right, $p8_table);
  $left = left_shift($left);
  $right = left_shift($right);
  $left = left_shift($left);
  $right = left_shift($right);
  $key2 = apply_table($left . $right, $p8_table);
  $temp = apply_table($message, $IP);
  $temp = f($expansion, $s0, $s1, $key1, $temp);
  $temp = substr($temp, 4, 8 - 4) . substr($temp, 0, 4 - 0);
  $temp = f($expansion, $s0, $s1, $key2, $temp);
  $CT = apply_table($temp, $IP_inv);
  echo rtrim('Cipher text is: ' . $CT), PHP_EOL;
  $temp = apply_table($CT, $IP);
  $temp = f($expansion, $s0, $s1, $key2, $temp);
  $temp = substr($temp, 4, 8 - 4) . substr($temp, 0, 4 - 0);
  $temp = f($expansion, $s0, $s1, $key1, $temp);
  $PT = apply_table($temp, $IP_inv);
  echo rtrim('Plain text after decypting is: ' . $PT), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
