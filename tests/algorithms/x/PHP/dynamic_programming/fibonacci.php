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
function create_fibonacci() {
  return ['sequence' => [0, 1]];
}
function fib_get(&$f, $index) {
  $seq = $f['sequence'];
  while (count($seq) < $index) {
  $next = $seq[count($seq) - 1] + $seq[count($seq) - 2];
  $seq = _append($seq, $next);
};
  $f['sequence'] = $seq;
  $result = [];
  $i = 0;
  while ($i < $index) {
  $result = _append($result, $seq[$i]);
  $i = $i + 1;
};
  return ['fib' => $f, 'values' => $result];
}
function main() {
  $fib = create_fibonacci();
  $res = fib_get($fib, 10);
  $fib = $res['fib'];
  echo rtrim(_str($res['values'])), PHP_EOL;
  $res = fib_get($fib, 5);
  $fib = $res['fib'];
  echo rtrim(_str($res['values'])), PHP_EOL;
}
main();
