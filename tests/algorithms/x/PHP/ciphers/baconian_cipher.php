<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$encode_map = ['a' => 'AAAAA', 'b' => 'AAAAB', 'c' => 'AAABA', 'd' => 'AAABB', 'e' => 'AABAA', 'f' => 'AABAB', 'g' => 'AABBA', 'h' => 'AABBB', 'i' => 'ABAAA', 'j' => 'BBBAA', 'k' => 'ABAAB', 'l' => 'ABABA', 'm' => 'ABABB', 'n' => 'ABBAA', 'o' => 'ABBAB', 'p' => 'ABBBA', 'q' => 'ABBBB', 'r' => 'BAAAA', 's' => 'BAAAB', 't' => 'BAABA', 'u' => 'BAABB', 'v' => 'BBBAB', 'w' => 'BABAA', 'x' => 'BABAB', 'y' => 'BABBA', 'z' => 'BABBB', ' ' => ' '];
function make_decode_map() {
  global $encode_map, $decode_map;
  $m = [];
  foreach (array_keys($encode_map) as $k) {
  $m[$encode_map[$k]] = $k;
};
  return $m;
}
$decode_map = make_decode_map();
function split_spaces($s) {
  global $encode_map, $decode_map;
  $parts = [];
  $current = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == ' ') {
  $parts = _append($parts, $current);
  $current = '';
} else {
  $current = $current . $ch;
}
  $i = $i + 1;
};
  $parts = _append($parts, $current);
  return $parts;
}
function encode($word) {
  global $encode_map, $decode_map;
  $w = strtolower($word);
  $encoded = '';
  $i = 0;
  while ($i < strlen($w)) {
  $ch = substr($w, $i, $i + 1 - $i);
  if (array_key_exists($ch, $encode_map)) {
  $encoded = $encoded . $encode_map[$ch];
} else {
  $panic('encode() accepts only letters of the alphabet and spaces');
}
  $i = $i + 1;
};
  return $encoded;
}
function decode($coded) {
  global $encode_map, $decode_map;
  $i = 0;
  while ($i < strlen($coded)) {
  $ch = substr($coded, $i, $i + 1 - $i);
  if ($ch != 'A' && $ch != 'B' && $ch != ' ') {
  $panic('decode() accepts only \'A\', \'B\' and spaces');
}
  $i = $i + 1;
};
  $words = split_spaces($coded);
  $decoded = '';
  $w = 0;
  while ($w < count($words)) {
  $word = $words[$w];
  $j = 0;
  while ($j < strlen($word)) {
  $segment = substr($word, $j, $j + 5 - $j);
  $decoded = $decoded . $decode_map[$segment];
  $j = $j + 5;
};
  if ($w < count($words) - 1) {
  $decoded = $decoded . ' ';
}
  $w = $w + 1;
};
  return $decoded;
}
echo rtrim(encode('hello')), PHP_EOL;
echo rtrim(encode('hello world')), PHP_EOL;
echo rtrim(decode('AABBBAABAAABABAABABAABBAB BABAAABBABBAAAAABABAAAABB')), PHP_EOL;
echo rtrim(decode('AABBBAABAAABABAABABAABBAB')), PHP_EOL;
