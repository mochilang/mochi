<?php
ini_set('memory_limit', '-1');
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
function nextRand($seed) {
  global $shuffleChars, $bestShuffle, $main;
  return ($seed * 1664525 + 1013904223) % 2147483647;
}
function shuffleChars($s, $seed) {
  global $nextRand, $bestShuffle, $main;
  $chars = [];
  $i = 0;
  while ($i < strlen($s)) {
  $chars = array_merge($chars, [substr($s, $i, $i + 1 - $i)]);
  $i = $i + 1;
};
  $sd = $seed;
  $idx = count($chars) - 1;
  while ($idx > 0) {
  $sd = nextRand($sd);
  $j = $sd % ($idx + 1);
  $tmp = $chars[$idx];
  $chars[$idx] = $chars[$j];
  $chars[$j] = $tmp;
  $idx = $idx - 1;
};
  $res = '';
  $i = 0;
  while ($i < count($chars)) {
  $res = $res . $chars[$i];
  $i = $i + 1;
};
  return [$res, $sd];
}
function bestShuffle($s, $seed) {
  global $nextRand, $shuffleChars, $main;
  $r = shuffleChars($s, $seed);
  $t = $r[0];
  $sd = $r[1];
  $arr = [];
  $i = 0;
  while ($i < _len($t)) {
  $arr = array_merge($arr, [substr($t, $i, $i + 1 - $i)]);
  $i = $i + 1;
};
  $i = 0;
  while ($i < count($arr)) {
  $j = 0;
  while ($j < count($arr)) {
  if ($i != $j && $arr[$i] != substr($s, $j, $j + 1 - $j) && $arr[$j] != substr($s, $i, $i + 1 - $i)) {
  $tmp = $arr[$i];
  $arr[$i] = $arr[$j];
  $arr[$j] = $tmp;
  break;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  $count = 0;
  $i = 0;
  while ($i < count($arr)) {
  if ($arr[$i] == substr($s, $i, $i + 1 - $i)) {
  $count = $count + 1;
}
  $i = $i + 1;
};
  $out = '';
  $i = 0;
  while ($i < count($arr)) {
  $out = $out . $arr[$i];
  $i = $i + 1;
};
  return [$out, $sd, $count];
}
function main() {
  global $nextRand, $shuffleChars, $bestShuffle;
  $ts = ['abracadabra', 'seesaw', 'elk', 'grrrrrr', 'up', 'a'];
  $seed = 1;
  $i = 0;
  while ($i < count($ts)) {
  $r = bestShuffle($ts[$i], $seed);
  $shuf = $r[0];
  $seed = $r[1];
  $cnt = $r[2];
  echo rtrim($ts[$i] . ' -> ' . $shuf . ' (' . json_encode($cnt, 1344) . ')'), PHP_EOL;
  $i = $i + 1;
};
}
main();
