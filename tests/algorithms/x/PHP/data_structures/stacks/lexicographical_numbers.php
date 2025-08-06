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
function lexical_order($max_number) {
  $result = [];
  $stack = [1];
  while (count($stack) > 0) {
  $idx = count($stack) - 1;
  $num = $stack[$idx];
  $stack = array_slice($stack, 0, $idx - 0);
  if ($num > $max_number) {
  continue;
}
  $result = _append($result, $num);
  if ($num % 10 != 9) {
  $stack = _append($stack, $num + 1);
}
  $stack = _append($stack, $num * 10);
};
  return $result;
}
function join_ints($xs) {
  $res = '';
  $i = 0;
  while ($i < count($xs)) {
  if ($i > 0) {
  $res = $res . ' ';
}
  $res = $res . _str($xs[$i]);
  $i = $i + 1;
};
  return $res;
}
echo rtrim(join_ints(lexical_order(13))), PHP_EOL;
echo rtrim(_str(lexical_order(1))), PHP_EOL;
echo rtrim(join_ints(lexical_order(20))), PHP_EOL;
echo rtrim(join_ints(lexical_order(25))), PHP_EOL;
echo rtrim(_str(lexical_order(12))), PHP_EOL;
