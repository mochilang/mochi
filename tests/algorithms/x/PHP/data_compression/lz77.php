<?php
ini_set('memory_limit', '-1');
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
function token_to_string($t) {
  global $c1, $c2, $tokens_example;
  return '(' . _str($t['offset']) . ', ' . _str($t['length']) . ', ' . $t['indicator'] . ')';
}
function tokens_to_string($ts) {
  global $c1, $c2, $tokens_example;
  $res = '[';
  $i = 0;
  while ($i < count($ts)) {
  $res = $res . token_to_string($ts[$i]);
  if ($i < count($ts) - 1) {
  $res = $res . ', ';
}
  $i = $i + 1;
};
  return $res . ']';
}
function match_length_from_index($text, $window, $text_index, $window_index) {
  global $c1, $c2, $tokens_example;
  if ($text_index >= strlen($text) || $window_index >= strlen($window)) {
  return 0;
}
  $tc = substr($text, $text_index, $text_index + 1 - $text_index);
  $wc = substr($window, $window_index, $window_index + 1 - $window_index);
  if ($tc != $wc) {
  return 0;
}
  return 1 + match_length_from_index($text, $window . $tc, $text_index + 1, $window_index + 1);
}
function find_encoding_token($text, $search_buffer) {
  global $c1, $c2, $tokens_example;
  if (strlen($text) == 0) {
  $panic('We need some text to work with.');
}
  $length = 0;
  $offset = 0;
  if (strlen($search_buffer) == 0) {
  return ['offset' => $offset, 'length' => $length, 'indicator' => substr($text, 0, 1 - 0)];
}
  $i = 0;
  while ($i < strlen($search_buffer)) {
  $ch = substr($search_buffer, $i, $i + 1 - $i);
  $found_offset = strlen($search_buffer) - $i;
  if ($ch == substr($text, 0, 1 - 0)) {
  $found_length = match_length_from_index($text, $search_buffer, 0, $i);
  if ($found_length >= $length) {
  $offset = $found_offset;
  $length = $found_length;
};
}
  $i = $i + 1;
};
  return ['offset' => $offset, 'length' => $length, 'indicator' => substr($text, $length, $length + 1 - $length)];
}
function lz77_compress($text, $window_size, $lookahead) {
  global $c1, $c2, $tokens_example;
  $search_buffer_size = $window_size - $lookahead;
  $output = [];
  $search_buffer = '';
  $remaining = $text;
  while (strlen($remaining) > 0) {
  $token = find_encoding_token($remaining, $search_buffer);
  $add_len = $token['length'] + 1;
  $search_buffer = $search_buffer . substr($remaining, 0, $add_len - 0);
  if (strlen($search_buffer) > $search_buffer_size) {
  $search_buffer = substr($search_buffer, strlen($search_buffer) - $search_buffer_size, strlen($search_buffer) - (strlen($search_buffer) - $search_buffer_size));
}
  $remaining = substr($remaining, $add_len, strlen($remaining) - $add_len);
  $output = _append($output, $token);
};
  return $output;
}
function lz77_decompress($tokens) {
  global $c1, $c2, $tokens_example;
  $output = '';
  foreach ($tokens as $t) {
  $i = 0;
  while ($i < $t['length']) {
  $output = $output . substr($output, strlen($output) - $t['offset'], strlen($output) - $t['offset'] + 1 - (strlen($output) - $t['offset']));
  $i = $i + 1;
};
  $output = $output . $t['indicator'];
};
  return $output;
}
$c1 = lz77_compress('ababcbababaa', 13, 6);
echo rtrim(tokens_to_string($c1)), PHP_EOL;
$c2 = lz77_compress('aacaacabcabaaac', 13, 6);
echo rtrim(tokens_to_string($c2)), PHP_EOL;
$tokens_example = [['offset' => 0, 'length' => 0, 'indicator' => 'c'], ['offset' => 0, 'length' => 0, 'indicator' => 'a'], ['offset' => 0, 'length' => 0, 'indicator' => 'b'], ['offset' => 0, 'length' => 0, 'indicator' => 'r'], ['offset' => 3, 'length' => 1, 'indicator' => 'c'], ['offset' => 2, 'length' => 1, 'indicator' => 'd'], ['offset' => 7, 'length' => 4, 'indicator' => 'r'], ['offset' => 3, 'length' => 5, 'indicator' => 'd']];
echo rtrim(lz77_decompress($tokens_example)), PHP_EOL;
