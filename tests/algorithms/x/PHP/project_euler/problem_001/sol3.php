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
function solution($n) {
  $total = 0;
  $num = 0;
  while (true) {
  $num = $num + 3;
  if ($num >= $n) {
  break;
}
  $total = $total + $num;
  $num = $num + 2;
  if ($num >= $n) {
  break;
}
  $total = $total + $num;
  $num = $num + 1;
  if ($num >= $n) {
  break;
}
  $total = $total + $num;
  $num = $num + 3;
  if ($num >= $n) {
  break;
}
  $total = $total + $num;
  $num = $num + 1;
  if ($num >= $n) {
  break;
}
  $total = $total + $num;
  $num = $num + 2;
  if ($num >= $n) {
  break;
}
  $total = $total + $num;
  $num = $num + 3;
  if ($num >= $n) {
  break;
}
  $total = $total + $num;
};
  return $total;
}
echo rtrim(_str(solution(1000))), PHP_EOL;
