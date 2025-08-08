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
  if ($n <= 1) {
  return 0;
}
  $a = 0;
  $b = 2;
  $count = 0;
  while (4 * $b + $a <= $n) {
  $next = 4 * $b + $a;
  $a = $b;
  $b = $next;
  $count = $count + $a;
};
  return $count + $b;
}
echo rtrim('solution() = ' . _str(solution(4000000))), PHP_EOL;
