<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function min_partitions($s) {
  $n = strlen($s);
  $cut = [];
  $i = 0;
  while ($i < $n) {
  $cut = _append($cut, 0);
  $i = $i + 1;
};
  $pal = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $row = _append($row, false);
  $j = $j + 1;
};
  $pal = _append($pal, $row);
  $i = $i + 1;
};
  $i = 0;
  while ($i < $n) {
  $mincut = $i;
  $j = 0;
  while ($j <= $i) {
  if (substr($s, $i, $i + 1 - $i) == substr($s, $j, $j + 1 - $j) && ($i - $j < 2 || $pal[$j + 1][$i - 1])) {
  $pal[$j][$i] = true;
  if ($j == 0) {
  $mincut = 0;
} else {
  $candidate = $cut[$j - 1] + 1;
  if ($candidate < $mincut) {
  $mincut = $candidate;
};
};
}
  $j = $j + 1;
};
  $cut[$i] = $mincut;
  $i = $i + 1;
};
  return $cut[$n - 1];
}
echo rtrim(json_encode(min_partitions('aab'), 1344)), PHP_EOL;
echo rtrim(json_encode(min_partitions('aaa'), 1344)), PHP_EOL;
echo rtrim(json_encode(min_partitions('ababbbabbababa'), 1344)), PHP_EOL;
