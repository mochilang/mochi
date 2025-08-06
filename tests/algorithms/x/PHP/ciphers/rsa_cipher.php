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
  $BYTE_SIZE = 256;
  function pow_int($base, $exp) {
  global $BYTE_SIZE;
  $result = 1;
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function mod_pow($base, $exponent, $modulus) {
  global $BYTE_SIZE;
  $result = 1;
  $b = $base % $modulus;
  $e = $exponent;
  while ($e > 0) {
  if ($e % 2 == 1) {
  $result = ($result * $b) % $modulus;
}
  $e = _intdiv($e, 2);
  $b = ($b * $b) % $modulus;
};
  return $result;
};
  function mochi_ord($ch) {
  global $BYTE_SIZE;
  if ($ch == ' ') {
  return 32;
}
  if ($ch == 'a') {
  return 97;
}
  if ($ch == 'b') {
  return 98;
}
  if ($ch == 'c') {
  return 99;
}
  if ($ch == 'd') {
  return 100;
}
  if ($ch == 'e') {
  return 101;
}
  if ($ch == 'f') {
  return 102;
}
  if ($ch == 'g') {
  return 103;
}
  if ($ch == 'h') {
  return 104;
}
  if ($ch == 'i') {
  return 105;
}
  if ($ch == 'j') {
  return 106;
}
  if ($ch == 'k') {
  return 107;
}
  if ($ch == 'l') {
  return 108;
}
  if ($ch == 'm') {
  return 109;
}
  if ($ch == 'n') {
  return 110;
}
  if ($ch == 'o') {
  return 111;
}
  if ($ch == 'p') {
  return 112;
}
  if ($ch == 'q') {
  return 113;
}
  if ($ch == 'r') {
  return 114;
}
  if ($ch == 's') {
  return 115;
}
  if ($ch == 't') {
  return 116;
}
  if ($ch == 'u') {
  return 117;
}
  if ($ch == 'v') {
  return 118;
}
  if ($ch == 'w') {
  return 119;
}
  if ($ch == 'x') {
  return 120;
}
  if ($ch == 'y') {
  return 121;
}
  if ($ch == 'z') {
  return 122;
}
  return 0;
};
  function mochi_chr($code) {
  global $BYTE_SIZE;
  if ($code == 32) {
  return ' ';
}
  if ($code == 97) {
  return 'a';
}
  if ($code == 98) {
  return 'b';
}
  if ($code == 99) {
  return 'c';
}
  if ($code == 100) {
  return 'd';
}
  if ($code == 101) {
  return 'e';
}
  if ($code == 102) {
  return 'f';
}
  if ($code == 103) {
  return 'g';
}
  if ($code == 104) {
  return 'h';
}
  if ($code == 105) {
  return 'i';
}
  if ($code == 106) {
  return 'j';
}
  if ($code == 107) {
  return 'k';
}
  if ($code == 108) {
  return 'l';
}
  if ($code == 109) {
  return 'm';
}
  if ($code == 110) {
  return 'n';
}
  if ($code == 111) {
  return 'o';
}
  if ($code == 112) {
  return 'p';
}
  if ($code == 113) {
  return 'q';
}
  if ($code == 114) {
  return 'r';
}
  if ($code == 115) {
  return 's';
}
  if ($code == 116) {
  return 't';
}
  if ($code == 117) {
  return 'u';
}
  if ($code == 118) {
  return 'v';
}
  if ($code == 119) {
  return 'w';
}
  if ($code == 120) {
  return 'x';
}
  if ($code == 121) {
  return 'y';
}
  if ($code == 122) {
  return 'z';
}
  return '';
};
  function get_blocks_from_text($message, $block_size) {
  global $BYTE_SIZE;
  $block_ints = [];
  $block_start = 0;
  while ($block_start < strlen($message)) {
  $block_int = 0;
  $i = $block_start;
  while ($i < $block_start + $block_size && $i < strlen($message)) {
  $block_int = $block_int + mochi_ord(substr($message, $i, $i + 1 - $i)) * pow_int($BYTE_SIZE, $i - $block_start);
  $i = $i + 1;
};
  $block_ints = _append($block_ints, $block_int);
  $block_start = $block_start + $block_size;
};
  return $block_ints;
};
  function get_text_from_blocks($block_ints, $message_length, $block_size) {
  global $BYTE_SIZE;
  $message = '';
  foreach ($block_ints as $block_int) {
  $block = $block_int;
  $i = $block_size - 1;
  $block_message = '';
  while ($i >= 0) {
  if (strlen($message) + $i < $message_length) {
  $ascii_number = $block / pow_int($BYTE_SIZE, $i);
  $block = fmod($block, pow_int($BYTE_SIZE, $i));
  $block_message = mochi_chr($ascii_number) . $block_message;
}
  $i = $i - 1;
};
  $message = $message . $block_message;
};
  return $message;
};
  function encrypt_message($message, $n, $e, $block_size) {
  global $BYTE_SIZE;
  $encrypted = [];
  $blocks = get_blocks_from_text($message, $block_size);
  foreach ($blocks as $block) {
  $encrypted = _append($encrypted, mod_pow($block, $e, $n));
};
  return $encrypted;
};
  function decrypt_message($blocks, $message_length, $n, $d, $block_size) {
  global $BYTE_SIZE;
  $decrypted_blocks = [];
  foreach ($blocks as $block) {
  $decrypted_blocks = _append($decrypted_blocks, mod_pow($block, $d, $n));
};
  $message = '';
  foreach ($decrypted_blocks as $num) {
  $message = $message . mochi_chr($num);
};
  return $message;
};
  function main() {
  global $BYTE_SIZE;
  $message = 'hello world';
  $n = 3233;
  $e = 17;
  $d = 2753;
  $block_size = 1;
  $encrypted = encrypt_message($message, $n, $e, $block_size);
  echo rtrim(_str($encrypted)), PHP_EOL;
  $decrypted = decrypt_message($encrypted, strlen($message), $n, $d, $block_size);
  echo rtrim($decrypted), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
