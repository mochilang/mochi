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
function row_string($row) {
  global $kmap;
  $s = '[';
  $i = 0;
  while ($i < count($row)) {
  $s = $s . _str($row[$i]);
  if ($i < count($row) - 1) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
}
function print_kmap($kmap) {
  $i = 0;
  while ($i < count($kmap)) {
  echo rtrim(row_string($kmap[$i])), PHP_EOL;
  $i = $i + 1;
};
}
function join_terms($terms) {
  global $kmap;
  if (count($terms) == 0) {
  return '';
}
  $res = $terms[0];
  $i = 1;
  while ($i < count($terms)) {
  $res = $res . ' + ' . $terms[$i];
  $i = $i + 1;
};
  return $res;
}
function simplify_kmap($board) {
  global $kmap;
  $terms = [];
  $a = 0;
  while ($a < count($board)) {
  $row = $board[$a];
  $b = 0;
  while ($b < count($row)) {
  $item = $row[$b];
  if ($item != 0) {
  $term = (($a != 0 ? 'A' : 'A\'')) . (($b != 0 ? 'B' : 'B\''));
  $terms = _append($terms, $term);
}
  $b = $b + 1;
};
  $a = $a + 1;
};
  $expr = join_terms($terms);
  return $expr;
}
$kmap = [[0, 1], [1, 1]];
print_kmap($kmap);
echo rtrim('Simplified Expression:'), PHP_EOL;
echo rtrim(simplify_kmap($kmap)), PHP_EOL;
