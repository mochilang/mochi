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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function solution($n) {
  if ($n <= 0) {
  _panic('Parameter n must be greater than or equal to one.');
}
  $a = 1;
  $b = 2;
  $total = 0;
  while ($a <= $n) {
  if ($a % 2 == 0) {
  $total = $total + $a;
}
  $c = $a + $b;
  $a = $b;
  $b = $c;
};
  return $total;
}
function main() {
  echo rtrim('solution() = ' . _str(solution(4000000))), PHP_EOL;
}
main();
