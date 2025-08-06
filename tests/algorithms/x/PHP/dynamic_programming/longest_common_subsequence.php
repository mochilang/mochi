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
function zeros_matrix($rows, $cols) {
  global $a, $b, $res;
  $matrix = [];
  $i = 0;
  while ($i <= $rows) {
  $row = [];
  $j = 0;
  while ($j <= $cols) {
  $row = _append($row, 0);
  $j = $j + 1;
};
  $matrix = _append($matrix, $row);
  $i = $i + 1;
};
  return $matrix;
}
function longest_common_subsequence($x, $y) {
  global $a, $b, $res;
  $m = strlen($x);
  $n = strlen($y);
  $dp = zeros_matrix($m, $n);
  $i = 1;
  while ($i <= $m) {
  $j = 1;
  while ($j <= $n) {
  if (substr($x, $i - 1, $i - 1 + 1 - ($i - 1)) == substr($y, $j - 1, $j - 1 + 1 - ($j - 1))) {
  $dp[$i][$j] = $dp[$i - 1][$j - 1] + 1;
} else {
  if ($dp[$i - 1][$j] > $dp[$i][$j - 1]) {
  $dp[$i][$j] = $dp[$i - 1][$j];
} else {
  $dp[$i][$j] = $dp[$i][$j - 1];
};
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  $seq = '';
  $i2 = $m;
  $j2 = $n;
  while ($i2 > 0 && $j2 > 0) {
  if (substr($x, $i2 - 1, $i2 - 1 + 1 - ($i2 - 1)) == substr($y, $j2 - 1, $j2 - 1 + 1 - ($j2 - 1))) {
  $seq = substr($x, $i2 - 1, $i2 - 1 + 1 - ($i2 - 1)) . $seq;
  $i2 = $i2 - 1;
  $j2 = $j2 - 1;
} else {
  if ($dp[$i2 - 1][$j2] >= $dp[$i2][$j2 - 1]) {
  $i2 = $i2 - 1;
} else {
  $j2 = $j2 - 1;
};
}
};
  return ['length' => $dp[$m][$n], 'sequence' => $seq];
}
$a = 'AGGTAB';
$b = 'GXTXAYB';
$res = longest_common_subsequence($a, $b);
echo rtrim('len = ' . _str($res['length']) . ', sub-sequence = ' . $res['sequence']), PHP_EOL;
