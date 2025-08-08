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
  $i = 0;
  while ($i < $n) {
  if ($i % 3 == 0 || $i % 5 == 0) {
  $total = $total + $i;
}
  $i = $i + 1;
};
  return $total;
}
echo rtrim(_str(solution(3))), PHP_EOL;
echo rtrim(_str(solution(4))), PHP_EOL;
echo rtrim(_str(solution(10))), PHP_EOL;
echo rtrim(_str(solution(600))), PHP_EOL;
