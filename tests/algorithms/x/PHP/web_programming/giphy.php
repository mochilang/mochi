<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function format_query($q) {
  $result = '';
  $i = 0;
  while ($i < strlen($q)) {
  $ch = substr($q, $i, $i + 1 - $i);
  if ($ch == ' ') {
  $result = $result . '+';
} else {
  $result = $result . $ch;
}
  $i = $i + 1;
};
  return $result;
}
function mochi_join($xs, $sep) {
  if (count($xs) == 0) {
  return '';
}
  $out = $xs[0];
  $i = 1;
  while ($i < count($xs)) {
  $out = $out . $sep . $xs[$i];
  $i = $i + 1;
};
  return $out;
}
function get_gifs($query) {
  $formatted = format_query($query);
  $gifs = json_decode(file_get_contents("tests/github/TheAlgorithms/Mochi/web_programming/giphy.json"), true);
  $urls = [];
  foreach ($gifs as $g) {
  $urls = _append($urls, $g['url']);
};
  return $urls;
}
echo rtrim(json_encode(mochi_join(get_gifs('space ship'), '
'), 1344)), PHP_EOL;
