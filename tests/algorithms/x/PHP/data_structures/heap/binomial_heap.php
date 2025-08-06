<?php
ini_set('memory_limit', '-1');
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function new_heap() {
  return ['data' => []];
}
function swap(&$data, $i, $j) {
  $tmp = $data[$i];
  $data[$i] = $data[$j];
  $data[$j] = $tmp;
}
function sift_up(&$data, $idx) {
  $i = $idx;
  while ($i > 0) {
  $parent = _intdiv(($i - 1), 2);
  if ($data[$parent] <= $data[$i]) {
  break;
}
  swap($data, $parent, $i);
  $i = $parent;
};
}
function sift_down(&$data, $idx) {
  $i = $idx;
  $n = count($data);
  while (true) {
  $left = 2 * $i + 1;
  $right = $left + 1;
  $smallest = $i;
  if ($left < $n && $data[$left] < $data[$smallest]) {
  $smallest = $left;
}
  if ($right < $n && $data[$right] < $data[$smallest]) {
  $smallest = $right;
}
  if ($smallest == $i) {
  break;
}
  swap($data, $i, $smallest);
  $i = $smallest;
};
}
function insert($heap, $v) {
  $d = $heap['data'];
  $d = _append($d, $v);
  sift_up($d, count($d) - 1);
  return ['data' => $d];
}
function peek($heap) {
  return $heap['data'][0];
}
function is_empty($heap) {
  return _len($heap['data']) == 0;
}
function delete_min($heap) {
  $d = $heap['data'];
  $min = $d[0];
  $d[0] = $d[count($d) - 1];
  $d = array_slice($d, 0, count($d) - 1 - 0);
  if (count($d) > 0) {
  sift_down($d, 0);
}
  return ['heap' => ['data' => $d], 'value' => $min];
}
function main() {
  $h = new_heap();
  $h = insert($h, 10);
  $h = insert($h, 3);
  $h = insert($h, 7);
  echo rtrim(_str(peek($h))), PHP_EOL;
  $d1 = delete_min($h);
  $h = $d1['heap'];
  echo rtrim(_str($d1['value'])), PHP_EOL;
  $d2 = delete_min($h);
  $h = $d2['heap'];
  echo rtrim(_str($d2['value'])), PHP_EOL;
  $d3 = delete_min($h);
  $h = $d3['heap'];
  echo rtrim(_str($d3['value'])), PHP_EOL;
}
main();
