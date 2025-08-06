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
function pow2($exp) {
  global $seq2, $seq1, $seq3;
  $result = 1;
  $i = 0;
  while ($i < $exp) {
  $result = $result * 2;
  $i = $i + 1;
};
  return $result;
}
function gray_code($bit_count) {
  global $seq2, $seq1, $seq3;
  if ($bit_count == 0) {
  return [0];
}
  $prev = gray_code($bit_count - 1);
  $add_val = pow2($bit_count - 1);
  $res = [];
  $i = 0;
  while ($i < count($prev)) {
  $res = _append($res, $prev[$i]);
  $i = $i + 1;
};
  $j = count($prev) - 1;
  while ($j >= 0) {
  $res = _append($res, $prev[$j] + $add_val);
  $j = $j - 1;
};
  return $res;
}
$seq2 = gray_code(2);
echo rtrim(_str($seq2)), PHP_EOL;
$seq1 = gray_code(1);
echo rtrim(_str($seq1)), PHP_EOL;
$seq3 = gray_code(3);
echo rtrim(_str($seq3)), PHP_EOL;
