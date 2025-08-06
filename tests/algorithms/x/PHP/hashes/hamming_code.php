<?php
ini_set('memory_limit', '-1');
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
function index_of($s, $ch) {
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function mochi_ord($ch) {
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $idx = index_of($upper, $ch);
  if ($idx >= 0) {
  return 65 + $idx;
}
  $idx = index_of($lower, $ch);
  if ($idx >= 0) {
  return 97 + $idx;
}
  return 0;
}
function mochi_chr($n) {
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  if ($n >= 65 && $n < 91) {
  return substr($upper, $n - 65, $n - 64 - ($n - 65));
}
  if ($n >= 97 && $n < 123) {
  return substr($lower, $n - 97, $n - 96 - ($n - 97));
}
  return '?';
}
function text_to_bits($text) {
  $bits = '';
  $i = 0;
  while ($i < strlen($text)) {
  $code = mochi_ord(substr($text, $i, $i + 1 - $i));
  $j = 7;
  while ($j >= 0) {
  $p = pow2($j);
  if (((_intdiv($code, $p)) % 2) == 1) {
  $bits = $bits . '1';
} else {
  $bits = $bits . '0';
}
  $j = $j - 1;
};
  $i = $i + 1;
};
  return $bits;
}
function text_from_bits($bits) {
  $text = '';
  $i = 0;
  while ($i < strlen($bits)) {
  $code = 0;
  $j = 0;
  while ($j < 8 && $i + $j < strlen($bits)) {
  $code = $code * 2;
  if (substr($bits, $i + $j, $i + $j + 1 - ($i + $j)) == '1') {
  $code = $code + 1;
}
  $j = $j + 1;
};
  $text = $text . mochi_chr($code);
  $i = $i + 8;
};
  return $text;
}
function bool_to_string($b) {
  if ($b) {
  return 'True';
}
  return 'False';
}
function string_to_bitlist($s) {
  $res = [];
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == '1') {
  $res = _append($res, 1);
} else {
  $res = _append($res, 0);
}
  $i = $i + 1;
};
  return $res;
}
function bitlist_to_string($bits) {
  $s = '';
  $i = 0;
  while ($i < count($bits)) {
  if ($bits[$i] == 1) {
  $s = $s . '1';
} else {
  $s = $s . '0';
}
  $i = $i + 1;
};
  return $s;
}
function is_power_of_two($x) {
  if ($x < 1) {
  return false;
}
  $p = 1;
  while ($p < $x) {
  $p = $p * 2;
};
  return $p == $x;
}
function list_eq($a, $b) {
  if (count($a) != count($b)) {
  return false;
}
  $i = 0;
  while ($i < count($a)) {
  if ($a[$i] != $b[$i]) {
  return false;
}
  $i = $i + 1;
};
  return true;
}
function pow2($e) {
  $res = 1;
  $i = 0;
  while ($i < $e) {
  $res = $res * 2;
  $i = $i + 1;
};
  return $res;
}
function has_bit($n, $b) {
  $p = pow2($b);
  if (((_intdiv($n, $p)) % 2) == 1) {
  return true;
}
  return false;
}
function hamming_encode($r, $data_bits) {
  $total = $r + count($data_bits);
  $data_ord = [];
  $cont_data = 0;
  $x = 1;
  while ($x <= $total) {
  if (is_power_of_two($x)) {
  $data_ord = _append($data_ord, -1);
} else {
  $data_ord = _append($data_ord, $data_bits[$cont_data]);
  $cont_data = $cont_data + 1;
}
  $x = $x + 1;
};
  $parity = [];
  $bp = 0;
  while ($bp < $r) {
  $cont_bo = 0;
  $j = 0;
  while ($j < count($data_ord)) {
  $bit = $data_ord[$j];
  if ($bit >= 0) {
  $pos = $j + 1;
  if (has_bit($pos, $bp) && $bit == 1) {
  $cont_bo = $cont_bo + 1;
};
}
  $j = $j + 1;
};
  $parity = _append($parity, $cont_bo % 2);
  $bp = $bp + 1;
};
  $result = [];
  $cont_bp = 0;
  $i = 0;
  while ($i < count($data_ord)) {
  if ($data_ord[$i] < 0) {
  $result = _append($result, $parity[$cont_bp]);
  $cont_bp = $cont_bp + 1;
} else {
  $result = _append($result, $data_ord[$i]);
}
  $i = $i + 1;
};
  return $result;
}
function hamming_decode($r, $code) {
  $data_output = [];
  $parity_received = [];
  $i = 1;
  $idx = 0;
  while ($i <= count($code)) {
  if (is_power_of_two($i)) {
  $parity_received = _append($parity_received, $code[$idx]);
} else {
  $data_output = _append($data_output, $code[$idx]);
}
  $idx = $idx + 1;
  $i = $i + 1;
};
  $recomputed = hamming_encode($r, $data_output);
  $parity_calc = [];
  $j = 0;
  while ($j < count($recomputed)) {
  if (is_power_of_two($j + 1)) {
  $parity_calc = _append($parity_calc, $recomputed[$j]);
}
  $j = $j + 1;
};
  $ack = list_eq($parity_received, $parity_calc);
  return ['data' => $data_output, 'ack' => $ack];
}
function main() {
  $sizePari = 4;
  $be = 2;
  $text = 'Message01';
  $binary = text_to_bits($text);
  echo rtrim('Text input in binary is \'' . $binary . '\''), PHP_EOL;
  $data_bits = string_to_bitlist($binary);
  $encoded = hamming_encode($sizePari, $data_bits);
  echo rtrim('Data converted ----------> ' . bitlist_to_string($encoded)), PHP_EOL;
  $decoded = hamming_decode($sizePari, $encoded);
  echo rtrim('Data receive ------------> ' . bitlist_to_string($decoded['data']) . ' -- Data integrity: ' . bool_to_string($decoded['ack'])), PHP_EOL;
  $corrupted = [];
  $i = 0;
  while ($i < count($encoded)) {
  $corrupted = _append($corrupted, $encoded[$i]);
  $i = $i + 1;
};
  $pos = $be - 1;
  if ($corrupted[$pos] == 0) {
  $corrupted[$pos] = 1;
} else {
  $corrupted[$pos] = 0;
}
  $decoded_err = hamming_decode($sizePari, $corrupted);
  echo rtrim('Data receive (error) ----> ' . bitlist_to_string($decoded_err['data']) . ' -- Data integrity: ' . bool_to_string($decoded_err['ack'])), PHP_EOL;
}
main();
