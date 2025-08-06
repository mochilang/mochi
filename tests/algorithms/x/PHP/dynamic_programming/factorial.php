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
$memo = [1, 1];
function factorial($num) {
  global $memo, $results;
  if ($num < 0) {
  echo rtrim('Number should not be negative.'), PHP_EOL;
  return 0;
}
  $m = $memo;
  $i = count($m);
  while ($i <= $num) {
  $m = _append($m, $i * $m[$i - 1]);
  $i = $i + 1;
};
  $memo = $m;
  return $m[$num];
}
echo rtrim(_str(factorial(7))), PHP_EOL;
factorial(-1);
$results = [];
for ($i = 0; $i < 10; $i++) {
  $results = _append($results, factorial($i));
}
echo rtrim(_str($results)), PHP_EOL;
