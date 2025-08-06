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
  $CHARS = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '&', '@', ':', ',', '.', '\'', '"', '?', '/', '=', '+', '-', '(', ')', '!', ' '];
  $CODES = ['.-', '-...', '-.-.', '-..', '.', '..-.', '--.', '....', '..', '.---', '-.-', '.-..', '--', '-.', '---', '.--.', '--.-', '.-.', '...', '-', '..-', '...-', '.--', '-..-', '-.--', '--..', '.----', '..---', '...--', '....-', '.....', '-....', '--...', '---..', '----.', '-----', '.-...', '.--.-.', '---...', '--..--', '.-.-.-', '.----.', '.-..-.', '..--..', '-..-.', '-...-', '.-.-.', '-....-', '-.--.', '-.--.-', '-.-.--', '/'];
  function to_upper_char($c) {
  global $CHARS, $CODES, $msg, $enc, $dec;
  if ($c == 'a') {
  return 'A';
}
  if ($c == 'b') {
  return 'B';
}
  if ($c == 'c') {
  return 'C';
}
  if ($c == 'd') {
  return 'D';
}
  if ($c == 'e') {
  return 'E';
}
  if ($c == 'f') {
  return 'F';
}
  if ($c == 'g') {
  return 'G';
}
  if ($c == 'h') {
  return 'H';
}
  if ($c == 'i') {
  return 'I';
}
  if ($c == 'j') {
  return 'J';
}
  if ($c == 'k') {
  return 'K';
}
  if ($c == 'l') {
  return 'L';
}
  if ($c == 'm') {
  return 'M';
}
  if ($c == 'n') {
  return 'N';
}
  if ($c == 'o') {
  return 'O';
}
  if ($c == 'p') {
  return 'P';
}
  if ($c == 'q') {
  return 'Q';
}
  if ($c == 'r') {
  return 'R';
}
  if ($c == 's') {
  return 'S';
}
  if ($c == 't') {
  return 'T';
}
  if ($c == 'u') {
  return 'U';
}
  if ($c == 'v') {
  return 'V';
}
  if ($c == 'w') {
  return 'W';
}
  if ($c == 'x') {
  return 'X';
}
  if ($c == 'y') {
  return 'Y';
}
  if ($c == 'z') {
  return 'Z';
}
  return $c;
};
  function to_upper($s) {
  global $CHARS, $CODES, $msg, $enc, $dec;
  $res = '';
  $i = 0;
  while ($i < strlen($s)) {
  $res = $res . to_upper_char(substr($s, $i, $i + 1 - $i));
  $i = $i + 1;
};
  return $res;
};
  function index_of($xs, $target) {
  global $CHARS, $CODES, $msg, $enc, $dec;
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $target) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function encrypt($message) {
  global $CHARS, $CODES, $enc, $dec;
  $msg = to_upper($message);
  $res = '';
  $i = 0;
  while ($i < strlen($msg)) {
  $c = substr($msg, $i, $i + 1 - $i);
  $idx = index_of($CHARS, $c);
  if ($idx >= 0) {
  if ($res != '') {
  $res = $res . ' ';
};
  $res = $res . $CODES[$idx];
}
  $i = $i + 1;
};
  return $res;
};
  function split_spaces($s) {
  global $CHARS, $CODES, $msg, $enc, $dec;
  $res = [];
  $current = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == ' ') {
  if ($current != '') {
  $res = _append($res, $current);
  $current = '';
};
} else {
  $current = $current . $ch;
}
  $i = $i + 1;
};
  if ($current != '') {
  $res = _append($res, $current);
}
  return $res;
};
  function decrypt($message) {
  global $CHARS, $CODES, $msg, $enc, $dec;
  $parts = split_spaces($message);
  $res = '';
  foreach ($parts as $code) {
  $idx = index_of($CODES, $code);
  if ($idx >= 0) {
  $res = $res . $CHARS[$idx];
}
};
  return $res;
};
  $msg = 'Morse code here!';
  echo rtrim($msg), PHP_EOL;
  $enc = encrypt($msg);
  echo rtrim($enc), PHP_EOL;
  $dec = decrypt($enc);
  echo rtrim($dec), PHP_EOL;
  echo rtrim(encrypt('Sos!')), PHP_EOL;
  echo rtrim(decrypt('... --- ... -.-.--')), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
