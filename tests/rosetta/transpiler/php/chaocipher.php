<?php
ini_set('memory_limit', '-1');
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
function indexOf($s, $ch) {
  global $rotate, $scrambleLeft, $scrambleRight, $chao, $main;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function rotate($s, $n) {
  global $indexOf, $scrambleLeft, $scrambleRight, $chao, $main;
  return substr($s, $n) . substr($s, 0, $n - 0);
}
function scrambleLeft($s) {
  global $indexOf, $rotate, $scrambleRight, $chao, $main;
  return substr($s, 0, 1 - 0) . substr($s, 2, 14 - 2) . substr($s, 1, 2 - 1) . substr($s, 14);
}
function scrambleRight($s) {
  global $indexOf, $rotate, $scrambleLeft, $chao, $main;
  return substr($s, 1, 3 - 1) . substr($s, 4, 15 - 4) . substr($s, 3, 4 - 3) . substr($s, 15) . substr($s, 0, 1 - 0);
}
function chao($text, $encode) {
  global $indexOf, $rotate, $scrambleLeft, $scrambleRight, $main;
  $left = 'HXUCZVAMDSLKPEFJRIGTWOBNYQ';
  $right = 'PTLNBQDEOYSFAVZKGJRIHWXUMC';
  $out = '';
  $i = 0;
  while ($i < strlen($text)) {
  $ch = substr($text, $i, $i + 1 - $i);
  $idx = 0;
  if ($encode) {
  $idx = _indexof($right, $ch);
  $out = $out . substr($left, $idx, $idx + 1 - $idx);
} else {
  $idx = _indexof($left, $ch);
  $out = $out . substr($right, $idx, $idx + 1 - $idx);
}
  $left = rotate($left, $idx);
  $right = rotate($right, $idx);
  $left = scrambleLeft($left);
  $right = scrambleRight($right);
  $i = $i + 1;
};
  return $out;
}
function main() {
  global $indexOf, $rotate, $scrambleLeft, $scrambleRight, $chao;
  $plain = 'WELLDONEISBETTERTHANWELLSAID';
  $cipher = chao($plain, true);
  echo rtrim($plain), PHP_EOL;
  echo rtrim($cipher), PHP_EOL;
  echo rtrim(chao($cipher, false)), PHP_EOL;
}
main();
