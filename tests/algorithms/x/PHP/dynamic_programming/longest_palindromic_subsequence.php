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
function reverse($s) {
  $result = '';
  $i = strlen($s) - 1;
  while ($i >= 0) {
  $result = $result . substr($s, $i, $i + 1 - $i);
  $i = $i - 1;
};
  return $result;
}
function max_int($a, $b) {
  if ($a > $b) {
  return $a;
}
  return $b;
}
function longest_palindromic_subsequence($s) {
  $rev = reverse($s);
  $n = strlen($s);
  $m = strlen($rev);
  $dp = [];
  $i = 0;
  while ($i <= $n) {
  $row = [];
  $j = 0;
  while ($j <= $m) {
  $row = _append($row, 0);
  $j = $j + 1;
};
  $dp = _append($dp, $row);
  $i = $i + 1;
};
  $i = 1;
  while ($i <= $n) {
  $j = 1;
  while ($j <= $m) {
  $a_char = substr($s, $i - 1, $i - ($i - 1));
  $b_char = substr($rev, $j - 1, $j - ($j - 1));
  if ($a_char == $b_char) {
  $dp[$i][$j] = 1 + $dp[$i - 1][$j - 1];
} else {
  $dp[$i][$j] = max_int($dp[$i - 1][$j], $dp[$i][$j - 1]);
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $dp[$n][$m];
}
echo rtrim(_str(longest_palindromic_subsequence('bbbab'))), PHP_EOL;
echo rtrim(_str(longest_palindromic_subsequence('bbabcbcab'))), PHP_EOL;
