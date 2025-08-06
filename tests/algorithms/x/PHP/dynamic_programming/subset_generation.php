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
function copy_list($src) {
  $result = [];
  $i = 0;
  while ($i < count($src)) {
  $result = _append($result, $src[$i]);
  $i = $i + 1;
};
  return $result;
}
function subset_combinations($elements, $n) {
  $r = count($elements);
  if ($n > $r) {
  return [];
}
  $dp = [];
  $i = 0;
  while ($i <= $r) {
  $dp = _append($dp, []);
  $i = $i + 1;
};
  $dp[0] = _append($dp[0], []);
  $i = 1;
  while ($i <= $r) {
  $j = $i;
  while ($j > 0) {
  $prevs = $dp[$j - 1];
  $k = 0;
  while ($k < count($prevs)) {
  $prev = $prevs[$k];
  $comb = copy_list($prev);
  $comb = _append($comb, $elements[$i - 1]);
  $dp[$j] = _append($dp[$j], $comb);
  $k = $k + 1;
};
  $j = $j - 1;
};
  $i = $i + 1;
};
  return $dp[$n];
}
echo rtrim(_str(subset_combinations([10, 20, 30, 40], 2))), PHP_EOL;
