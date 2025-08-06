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
function min3($a, $b, $c) {
  $m = $a;
  if ($b < $m) {
  $m = $b;
}
  if ($c < $m) {
  $m = $c;
}
  return $m;
}
function helper($word1, $word2, &$cache, $i, $j, $len1, $len2) {
  if ($i >= $len1) {
  return $len2 - $j;
}
  if ($j >= $len2) {
  return $len1 - $i;
}
  if ($cache[$i][$j] != (0 - 1)) {
  return $cache[$i][$j];
}
  $diff = 0;
  if (substr($word1, $i, $i + 1 - $i) != substr($word2, $j, $j + 1 - $j)) {
  $diff = 1;
}
  $delete_cost = 1 + helper($word1, $word2, $cache, $i + 1, $j, $len1, $len2);
  $insert_cost = 1 + helper($word1, $word2, $cache, $i, $j + 1, $len1, $len2);
  $replace_cost = $diff + helper($word1, $word2, $cache, $i + 1, $j + 1, $len1, $len2);
  $cache[$i][$j] = min3($delete_cost, $insert_cost, $replace_cost);
  return $cache[$i][$j];
}
function min_distance_up_bottom($word1, $word2) {
  $len1 = strlen($word1);
  $len2 = strlen($word2);
  $cache = [];
  for ($_ = 0; $_ < $len1; $_++) {
  $row = [];
  for ($_2 = 0; $_2 < $len2; $_2++) {
  $row = _append($row, 0 - 1);
};
  $cache = _append($cache, $row);
};
  return helper($word1, $word2, $cache, 0, 0, $len1, $len2);
}
echo rtrim(_str(min_distance_up_bottom('intention', 'execution'))), PHP_EOL;
echo rtrim(_str(min_distance_up_bottom('intention', ''))), PHP_EOL;
echo rtrim(_str(min_distance_up_bottom('', ''))), PHP_EOL;
echo rtrim(_str(min_distance_up_bottom('zooicoarchaeologist', 'zoologist'))), PHP_EOL;
