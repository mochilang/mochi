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
  $a = 0;
  $b = 1;
  $total = 0;
  while ($b <= $n) {
  if ($b % 2 == 0) {
  $total = $total + $b;
}
  $next_val = $a + $b;
  $a = $b;
  $b = $next_val;
};
  return $total;
}
echo rtrim(_str(solution(10))), PHP_EOL;
echo rtrim(_str(solution(15))), PHP_EOL;
echo rtrim(_str(solution(2))), PHP_EOL;
echo rtrim(_str(solution(1))), PHP_EOL;
echo rtrim(_str(solution(34))), PHP_EOL;
echo rtrim(_str(solution(4000000))), PHP_EOL;
