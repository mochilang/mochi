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
function find_minimum_change($denominations, $value) {
  if ($value <= 0) {
  return [];
}
  $total = $value;
  $answer = [];
  $i = count($denominations) - 1;
  while ($i >= 0) {
  $denom = $denominations[$i];
  while ($total >= $denom) {
  $total = $total - $denom;
  $answer = _append($answer, $denom);
};
  $i = $i - 1;
};
  return $answer;
}
echo rtrim(_str(find_minimum_change([1, 2, 5, 10, 20, 50, 100, 500, 2000], 987))), PHP_EOL;
echo rtrim(_str(find_minimum_change([1, 5, 100, 500, 1000], 456))), PHP_EOL;
