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
  $triagrams = ['111', '112', '113', '121', '122', '123', '131', '132', '133', '211', '212', '213', '221', '222', '223', '231', '232', '233', '311', '312', '313', '321', '322', '323', '331', '332', '333'];
  function remove_spaces($s) {
  global $triagrams;
  $res = '';
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if ($c != ' ') {
  $res = $res . $c;
}
  $i = $i + 1;
};
  return $res;
};
  function char_to_trigram($ch, $alphabet) {
  global $triagrams;
  $i = 0;
  while ($i < strlen($alphabet)) {
  if (substr($alphabet, $i, $i + 1 - $i) == $ch) {
  return $triagrams[$i];
}
  $i = $i + 1;
};
  return '';
};
  function trigram_to_char($tri, $alphabet) {
  global $triagrams;
  $i = 0;
  while ($i < count($triagrams)) {
  if ($triagrams[$i] == $tri) {
  return substr($alphabet, $i, $i + 1 - $i);
}
  $i = $i + 1;
};
  return '';
};
  function encrypt_part($part, $alphabet) {
  global $triagrams;
  $one = '';
  $two = '';
  $three = '';
  $i = 0;
  while ($i < strlen($part)) {
  $tri = char_to_trigram(substr($part, $i, $i + 1 - $i), $alphabet);
  $one = $one . substr($tri, 0, 1 - 0);
  $two = $two . substr($tri, 1, 2 - 1);
  $three = $three . substr($tri, 2, 3 - 2);
  $i = $i + 1;
};
  return $one . $two . $three;
};
  function encrypt_message($message, $alphabet, $period) {
  global $triagrams;
  $msg = remove_spaces($message);
  $alpha = remove_spaces($alphabet);
  if (strlen($alpha) != 27) {
  return '';
}
  $encrypted_numeric = '';
  $i = 0;
  while ($i < strlen($msg)) {
  $end = $i + $period;
  if ($end > strlen($msg)) {
  $end = strlen($msg);
}
  $part = substr($msg, $i, $end - $i);
  $encrypted_numeric = $encrypted_numeric . encrypt_part($part, $alpha);
  $i = $i + $period;
};
  $encrypted = '';
  $j = 0;
  while ($j < strlen($encrypted_numeric)) {
  $tri = substr($encrypted_numeric, $j, $j + 3 - $j);
  $encrypted = $encrypted . trigram_to_char($tri, $alpha);
  $j = $j + 3;
};
  return $encrypted;
};
  function decrypt_part($part, $alphabet) {
  global $triagrams;
  $converted = '';
  $i = 0;
  while ($i < strlen($part)) {
  $tri = char_to_trigram(substr($part, $i, $i + 1 - $i), $alphabet);
  $converted = $converted . $tri;
  $i = $i + 1;
};
  $result = [];
  $tmp = '';
  $j = 0;
  while ($j < strlen($converted)) {
  $tmp = $tmp . substr($converted, $j, $j + 1 - $j);
  if (strlen($tmp) == strlen($part)) {
  $result = _append($result, $tmp);
  $tmp = '';
}
  $j = $j + 1;
};
  return $result;
};
  function decrypt_message($message, $alphabet, $period) {
  global $triagrams;
  $msg = remove_spaces($message);
  $alpha = remove_spaces($alphabet);
  if (strlen($alpha) != 27) {
  return '';
}
  $decrypted_numeric = [];
  $i = 0;
  while ($i < strlen($msg)) {
  $end = $i + $period;
  if ($end > strlen($msg)) {
  $end = strlen($msg);
}
  $part = substr($msg, $i, $end - $i);
  $groups = decrypt_part($part, $alpha);
  $k = 0;
  while ($k < strlen($groups[0])) {
  $tri = substr($groups[0], $k, $k + 1 - $k) . substr($groups[1], $k, $k + 1 - $k) . substr($groups[2], $k, $k + 1 - $k);
  $decrypted_numeric = _append($decrypted_numeric, $tri);
  $k = $k + 1;
};
  $i = $i + $period;
};
  $decrypted = '';
  $j = 0;
  while ($j < count($decrypted_numeric)) {
  $decrypted = $decrypted . trigram_to_char($decrypted_numeric[$j], $alpha);
  $j = $j + 1;
};
  return $decrypted;
};
  function main() {
  global $triagrams;
  $msg = 'DEFEND THE EAST WALL OF THE CASTLE.';
  $alphabet = 'EPSDUCVWYM.ZLKXNBTFGORIJHAQ';
  $encrypted = encrypt_message($msg, $alphabet, 5);
  $decrypted = decrypt_message($encrypted, $alphabet, 5);
  echo rtrim('Encrypted: ' . $encrypted), PHP_EOL;
  echo rtrim('Decrypted: ' . $decrypted), PHP_EOL;
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
