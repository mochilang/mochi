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
function helper_top_down($word1, $word2, &$dp, $i, $j) {
  if ($i < 0) {
  return $j + 1;
}
  if ($j < 0) {
  return $i + 1;
}
  if ($dp[$i][$j] != (0 - 1)) {
  return $dp[$i][$j];
}
  if (substr($word1, $i, $i + 1 - $i) == substr($word2, $j, $j + 1 - $j)) {
  $dp[$i][$j] = helper_top_down($word1, $word2, $dp, $i - 1, $j - 1);
} else {
  $insert = helper_top_down($word1, $word2, $dp, $i, $j - 1);
  $delete = helper_top_down($word1, $word2, $dp, $i - 1, $j);
  $replace = helper_top_down($word1, $word2, $dp, $i - 1, $j - 1);
  $dp[$i][$j] = 1 + min3($insert, $delete, $replace);
}
  return $dp[$i][$j];
}
function min_dist_top_down($word1, $word2) {
  $m = strlen($word1);
  $n = strlen($word2);
  $dp = [];
  for ($_ = 0; $_ < $m; $_++) {
  $row = [];
  for ($_2 = 0; $_2 < $n; $_2++) {
  $row = _append($row, 0 - 1);
};
  $dp = _append($dp, $row);
};
  return helper_top_down($word1, $word2, $dp, $m - 1, $n - 1);
}
function min_dist_bottom_up($word1, $word2) {
  $m = strlen($word1);
  $n = strlen($word2);
  $dp = [];
  for ($_ = 0; $_ < ($m + 1); $_++) {
  $row = [];
  for ($_2 = 0; $_2 < ($n + 1); $_2++) {
  $row = _append($row, 0);
};
  $dp = _append($dp, $row);
};
  for ($i = 0; $i < ($m + 1); $i++) {
  for ($j = 0; $j < ($n + 1); $j++) {
  if ($i == 0) {
  $dp[$i][$j] = $j;
} else {
  if ($j == 0) {
  $dp[$i][$j] = $i;
} else {
  if (substr($word1, $i - 1, $i - ($i - 1)) == substr($word2, $j - 1, $j - ($j - 1))) {
  $dp[$i][$j] = $dp[$i - 1][$j - 1];
} else {
  $insert = $dp[$i][$j - 1];
  $delete = $dp[$i - 1][$j];
  $replace = $dp[$i - 1][$j - 1];
  $dp[$i][$j] = 1 + min3($insert, $delete, $replace);
};
};
}
};
};
  return $dp[$m][$n];
}
echo rtrim(_str(min_dist_top_down('intention', 'execution'))), PHP_EOL;
echo rtrim(_str(min_dist_top_down('intention', ''))), PHP_EOL;
echo rtrim(_str(min_dist_top_down('', ''))), PHP_EOL;
echo rtrim(_str(min_dist_bottom_up('intention', 'execution'))), PHP_EOL;
echo rtrim(_str(min_dist_bottom_up('intention', ''))), PHP_EOL;
echo rtrim(_str(min_dist_bottom_up('', ''))), PHP_EOL;
