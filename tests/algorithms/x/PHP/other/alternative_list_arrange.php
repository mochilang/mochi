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
function from_int($x) {
  global $example1, $example2, $example3, $example4;
  return ['__tag' => 'Int', 'value' => $x];
}
function from_string($s) {
  global $example1, $example2, $example3, $example4;
  return ['__tag' => 'Str', 'value' => $s];
}
function item_to_string($it) {
  global $example1, $example2, $example3, $example4;
  return (function($__v) {
  if ($__v['__tag'] === "Int") {
    $v = $__v["value"];
    return _str($v);
  } elseif ($__v['__tag'] === "Str") {
    $s = $__v["value"];
    return $s;
  }
})($it);
}
function alternative_list_arrange($first, $second) {
  global $example1, $example2, $example3, $example4;
  $len1 = count($first);
  $len2 = count($second);
  $abs_len = ($len1 > $len2 ? $len1 : $len2);
  $result = [];
  $i = 0;
  while ($i < $abs_len) {
  if ($i < $len1) {
  $result = _append($result, $first[$i]);
}
  if ($i < $len2) {
  $result = _append($result, $second[$i]);
}
  $i = $i + 1;
};
  return $result;
}
function list_to_string($xs) {
  global $example1, $example2, $example3, $example4;
  $s = '[';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . item_to_string($xs[$i]);
  if ($i < count($xs) - 1) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
}
$example1 = alternative_list_arrange([from_int(1), from_int(2), from_int(3), from_int(4), from_int(5)], [from_string('A'), from_string('B'), from_string('C')]);
echo rtrim(list_to_string($example1)), PHP_EOL;
$example2 = alternative_list_arrange([from_string('A'), from_string('B'), from_string('C')], [from_int(1), from_int(2), from_int(3), from_int(4), from_int(5)]);
echo rtrim(list_to_string($example2)), PHP_EOL;
$example3 = alternative_list_arrange([from_string('X'), from_string('Y'), from_string('Z')], [from_int(9), from_int(8), from_int(7), from_int(6)]);
echo rtrim(list_to_string($example3)), PHP_EOL;
$example4 = alternative_list_arrange([from_int(1), from_int(2), from_int(3), from_int(4), from_int(5)], []);
echo rtrim(list_to_string($example4)), PHP_EOL;
