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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  $KEY_STRING = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  function mod36($n) {
  global $KEY_STRING, $key;
  $r = $n % 36;
  if ($r < 0) {
  $r = $r + 36;
}
  return $r;
};
  function gcd($a, $b) {
  global $KEY_STRING, $key;
  $x = $a;
  $y = $b;
  while ($y != 0) {
  $t = $y;
  $y = $x % $y;
  $x = $t;
};
  if ($x < 0) {
  $x = -$x;
}
  return $x;
};
  function replace_letters($letter) {
  global $KEY_STRING, $key;
  $i = 0;
  while ($i < strlen($KEY_STRING)) {
  if (substr($KEY_STRING, $i, $i + 1 - $i) == $letter) {
  return $i;
}
  $i = $i + 1;
};
  return 0;
};
  function replace_digits($num) {
  global $KEY_STRING, $key;
  $idx = mod36($num);
  return substr($KEY_STRING, $idx, $idx + 1 - $idx);
};
  function to_upper($c) {
  global $KEY_STRING, $key;
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $i = 0;
  while ($i < strlen($lower)) {
  if ($c == substr($lower, $i, $i + 1 - $i)) {
  return substr($upper, $i, $i + 1 - $i);
}
  $i = $i + 1;
};
  return $c;
};
  function process_text($text, $break_key) {
  global $KEY_STRING, $key;
  $chars = [];
  $i = 0;
  while ($i < strlen($text)) {
  $c = to_upper(substr($text, $i, $i + 1 - $i));
  $j = 0;
  $ok = false;
  while ($j < strlen($KEY_STRING)) {
  if (substr($KEY_STRING, $j, $j + 1 - $j) == $c) {
  $ok = true;
  break;
}
  $j = $j + 1;
};
  if ($ok) {
  $chars = _append($chars, $c);
}
  $i = $i + 1;
};
  if (count($chars) == 0) {
  return '';
}
  $last = $chars[count($chars) - 1];
  while (fmod(count($chars), $break_key) != 0) {
  $chars = _append($chars, $last);
};
  $res = '';
  $k = 0;
  while ($k < count($chars)) {
  $res = $res . $chars[$k];
  $k = $k + 1;
};
  return $res;
};
  function matrix_minor($m, $row, $col) {
  global $KEY_STRING, $key;
  $res = [];
  $i = 0;
  while ($i < count($m)) {
  if ($i != $row) {
  $r = [];
  $j = 0;
  while ($j < count($m[$i])) {
  if ($j != $col) {
  $r = _append($r, $m[$i][$j]);
}
  $j = $j + 1;
};
  $res = _append($res, $r);
}
  $i = $i + 1;
};
  return $res;
};
  function determinant($m) {
  global $KEY_STRING, $key;
  $n = count($m);
  if ($n == 1) {
  return $m[0][0];
}
  if ($n == 2) {
  return $m[0][0] * $m[1][1] - $m[0][1] * $m[1][0];
}
  $det = 0;
  $col = 0;
  while ($col < $n) {
  $minor_mat = matrix_minor($m, 0, $col);
  $sign = 1;
  if ($col % 2 == 1) {
  $sign = -1;
}
  $det = $det + $sign * $m[0][$col] * determinant($minor_mat);
  $col = $col + 1;
};
  return $det;
};
  function cofactor_matrix($m) {
  global $KEY_STRING, $key;
  $n = count($m);
  $res = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $minor_mat = matrix_minor($m, $i, $j);
  $det_minor = determinant($minor_mat);
  $sign = 1;
  if (($i + $j) % 2 == 1) {
  $sign = -1;
}
  $row = _append($row, $sign * $det_minor);
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return $res;
};
  function transpose($m) {
  global $KEY_STRING, $key;
  $rows = count($m);
  $cols = count($m[0]);
  $res = [];
  $j = 0;
  while ($j < $cols) {
  $row = [];
  $i = 0;
  while ($i < $rows) {
  $row = _append($row, $m[$i][$j]);
  $i = $i + 1;
};
  $res = _append($res, $row);
  $j = $j + 1;
};
  return $res;
};
  function matrix_mod($m) {
  global $KEY_STRING, $key;
  $res = [];
  $i = 0;
  while ($i < count($m)) {
  $row = [];
  $j = 0;
  while ($j < count($m[$i])) {
  $row = _append($row, mod36($m[$i][$j]));
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return $res;
};
  function scalar_matrix_mult($s, $m) {
  global $KEY_STRING, $key;
  $res = [];
  $i = 0;
  while ($i < count($m)) {
  $row = [];
  $j = 0;
  while ($j < count($m[$i])) {
  $row = _append($row, mod36($s * $m[$i][$j]));
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return $res;
};
  function adjugate($m) {
  global $KEY_STRING, $key;
  $cof = cofactor_matrix($m);
  $n = count($cof);
  $res = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $row = _append($row, $cof[$j][$i]);
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return $res;
};
  function multiply_matrix_vector($m, $v) {
  global $KEY_STRING, $key;
  $n = count($m);
  $res = [];
  $i = 0;
  while ($i < $n) {
  $sum = 0;
  $j = 0;
  while ($j < $n) {
  $sum = $sum + $m[$i][$j] * $v[$j];
  $j = $j + 1;
};
  $res = _append($res, mod36($sum));
  $i = $i + 1;
};
  return $res;
};
  function inverse_key($key) {
  global $KEY_STRING;
  $det_val = determinant($key);
  $det_mod = mod36($det_val);
  $det_inv = 0;
  $i = 0;
  while ($i < 36) {
  if (($det_mod * $i) % 36 == 1) {
  $det_inv = $i;
  break;
}
  $i = $i + 1;
};
  $adj = adjugate($key);
  $tmp = scalar_matrix_mult($det_inv, $adj);
  $res = matrix_mod($tmp);
  return $res;
};
  function hill_encrypt($key, $text) {
  global $KEY_STRING;
  $break_key = count($key);
  $processed = process_text($text, $break_key);
  $encrypted = '';
  $i = 0;
  while ($i < strlen($processed)) {
  $vec = [];
  $j = 0;
  while ($j < $break_key) {
  $vec = _append($vec, replace_letters($processed[$i + $j]));
  $j = $j + 1;
};
  $enc_vec = multiply_matrix_vector($key, $vec);
  $k = 0;
  while ($k < $break_key) {
  $encrypted = $encrypted . replace_digits($enc_vec[$k]);
  $k = $k + 1;
};
  $i = $i + $break_key;
};
  return $encrypted;
};
  function hill_decrypt($key, $text) {
  global $KEY_STRING;
  $break_key = count($key);
  $decrypt_key = inverse_key($key);
  $processed = process_text($text, $break_key);
  $decrypted = '';
  $i = 0;
  while ($i < strlen($processed)) {
  $vec = [];
  $j = 0;
  while ($j < $break_key) {
  $vec = _append($vec, replace_letters($processed[$i + $j]));
  $j = $j + 1;
};
  $dec_vec = multiply_matrix_vector($decrypt_key, $vec);
  $k = 0;
  while ($k < $break_key) {
  $decrypted = $decrypted . replace_digits($dec_vec[$k]);
  $k = $k + 1;
};
  $i = $i + $break_key;
};
  return $decrypted;
};
  $key = [[2, 5], [1, 6]];
  echo rtrim(hill_encrypt($key, 'testing hill cipher')), PHP_EOL;
  echo rtrim(hill_encrypt($key, 'hello')), PHP_EOL;
  echo rtrim(hill_decrypt($key, 'WHXYJOLM9C6XT085LL')), PHP_EOL;
  echo rtrim(hill_decrypt($key, '85FF00')), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
