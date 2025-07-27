<?php
ini_set('memory_limit', '-1');
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
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
function mochi_join($xs, $sep) {
  $res = '';
  $i = 0;
  while ($i < count($xs)) {
  if ($i > 0) {
  $res = $res . $sep;
}
  $res = $res . $xs[$i];
  $i = $i + 1;
};
  return $res;
}
function sentenceType($s) {
  if (strlen($s) == 0) {
  return '';
}
  $types = [];
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == '?') {
  $types = array_merge($types, ['Q']);
} else {
  if ($ch == '!') {
  $types = array_merge($types, ['E']);
} else {
  if ($ch == '.') {
  $types = array_merge($types, ['S']);
};
};
}
  $i = $i + 1;
};
  $last = substr($s, strlen($s) - 1, strlen($s) - (strlen($s) - 1));
  if (_indexof('?!.', $last) == (-1)) {
  $types = array_merge($types, ['N']);
}
  return mochi_join($types, '|');
}
function main() {
  $s = 'hi there, how are you today? I\'d like to present to you the washing machine 9001. You have been nominated to win one of these! Just make sure you don\'t break it';
  $result = sentenceType($s);
  echo rtrim($result), PHP_EOL;
}
main();
