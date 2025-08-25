<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function build_set($words) {
  $m = [];
  foreach ($words as $w) {
  $m[$w] = true;
};
  return $m;
}
function word_break($s, $words) {
  $n = strlen($s);
  $dict = build_set($words);
  $dp = [];
  $i = 0;
  while ($i <= $n) {
  $dp = _append($dp, false);
  $i = $i + 1;
};
  $dp[0] = true;
  $i = 1;
  while ($i <= $n) {
  $j = 0;
  while ($j < $i) {
  if ($dp[$j]) {
  $sub = substr($s, $j, $i - $j);
  if (array_key_exists($sub, $dict)) {
  $dp[$i] = true;
  $j = $i;
};
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $dp[$n];
}
function print_bool($b) {
  if ($b) {
  echo rtrim((true ? 'true' : 'false')), PHP_EOL;
} else {
  echo rtrim((false ? 'true' : 'false')), PHP_EOL;
}
}
print_bool(word_break('applepenapple', ['apple', 'pen']));
print_bool(word_break('catsandog', ['cats', 'dog', 'sand', 'and', 'cat']));
print_bool(word_break('cars', ['car', 'ca', 'rs']));
