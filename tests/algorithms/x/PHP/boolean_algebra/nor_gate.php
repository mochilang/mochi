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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function nor_gate($input_1, $input_2) {
  if ($input_1 == 0 && $input_2 == 0) {
  return 1;
}
  return 0;
}
function center($s, $width) {
  $total = $width - strlen($s);
  if ($total <= 0) {
  return $s;
}
  $left = _intdiv($total, 2);
  $right = $total - $left;
  $res = $s;
  $i = 0;
  while ($i < $left) {
  $res = ' ' . $res;
  $i = $i + 1;
};
  $j = 0;
  while ($j < $right) {
  $res = $res . ' ';
  $j = $j + 1;
};
  return $res;
}
function make_table_row($i, $j) {
  $output = nor_gate($i, $j);
  return '| ' . center(_str($i), 8) . ' | ' . center(_str($j), 8) . ' | ' . center(_str($output), 8) . ' |';
}
function truth_table() {
  return 'Truth Table of NOR Gate:
' . '| Input 1 | Input 2 | Output  |
' . make_table_row(0, 0) . '
' . make_table_row(0, 1) . '
' . make_table_row(1, 0) . '
' . make_table_row(1, 1);
}
echo rtrim(json_encode(nor_gate(0, 0), 1344)), PHP_EOL;
echo rtrim(json_encode(nor_gate(0, 1), 1344)), PHP_EOL;
echo rtrim(json_encode(nor_gate(1, 0), 1344)), PHP_EOL;
echo rtrim(json_encode(nor_gate(1, 1), 1344)), PHP_EOL;
echo rtrim(truth_table()), PHP_EOL;
