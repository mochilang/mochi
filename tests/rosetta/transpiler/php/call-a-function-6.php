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
function bar($a, $b, $c) {
  global $main;
  echo rtrim(_str($a) . ', ' . _str($b) . ', ' . _str($c)), PHP_EOL;
}
function main() {
  global $bar;
  $args = [];
  $args['a'] = 3;
  $args['b'] = 2;
  $args['c'] = 1;
  bar($args['a'], $args['b'], $args['c']);
}
main();
