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
function mkAdd($a) {
  global $mysum, $partialSum, $main;
  return function($b) use ($a) {
  return $a + $b;
};
}
function mysum($x, $y) {
  global $mkAdd, $partialSum, $main;
  return $x + $y;
}
function partialSum($x) {
  global $mkAdd, $mysum, $main;
  return function($y) use ($x, &$mkAdd, &$mysum) {
  return mysum($x, $y);
};
}
function main() {
  global $mkAdd, $mysum, $partialSum;
  $add2 = mkAdd(2);
  $add3 = mkAdd(3);
  echo rtrim(_str($add2(5)) . ' ' . _str($add3(6))), PHP_EOL;
  $partial = partialSum(13);
  echo rtrim(json_encode(_str($partial(5)), 1344)), PHP_EOL;
}
main();
