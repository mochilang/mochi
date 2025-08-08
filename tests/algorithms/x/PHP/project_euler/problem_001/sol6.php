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
  $a = 3;
  $result = 0;
  while ($a < $n) {
  if ($a % 3 == 0 || $a % 5 == 0) {
  $result = $result + $a;
} else {
  if ($a % 15 == 0) {
  $result = $result - $a;
};
}
  $a = $a + 1;
};
  return $result;
}
function main() {
  echo rtrim('solution() = ' . _str(solution(1000))), PHP_EOL;
}
main();
