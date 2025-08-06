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
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function indexOf($s, $ch) {
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function charToNum($ch) {
  $letters = 'abcdefghijklmnopqrstuvwxyz';
  $idx = _indexof($letters, $ch);
  if ($idx >= 0) {
  return $idx + 1;
}
  return 0;
}
function numToChar($n) {
  $letters = 'abcdefghijklmnopqrstuvwxyz';
  if ($n >= 1 && $n <= 26) {
  return substr($letters, $n - 1, $n - ($n - 1));
}
  return '?';
}
function encode($plain) {
  $res = [];
  $i = 0;
  while ($i < strlen($plain)) {
  $ch = strtolower(substr($plain, $i, $i + 1 - $i));
  $val = charToNum($ch);
  if ($val > 0) {
  $res = _append($res, $val);
}
  $i = $i + 1;
};
  return $res;
}
function decode($encoded) {
  $out = '';
  foreach ($encoded as $n) {
  $out = $out . numToChar($n);
};
  return $out;
}
function main() {
  echo rtrim('-> '), PHP_EOL;
  $text = strtolower(trim(fgets(STDIN)));
  $enc = encode($text);
  echo rtrim('Encoded: ' . _str($enc)), PHP_EOL;
  echo rtrim('Decoded: ' . decode($enc)), PHP_EOL;
}
main();
