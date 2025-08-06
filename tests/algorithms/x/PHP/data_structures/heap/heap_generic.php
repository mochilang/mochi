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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function new_heap($key) {
  global $h;
  return ['arr' => [], 'pos_map' => [], 'size' => 0, 'key' => $key];
}
function parent($i) {
  global $h;
  if ($i > 0) {
  return _intdiv(($i - 1), 2);
}
  return -1;
}
function left($i, $size) {
  global $h;
  $l = 2 * $i + 1;
  if ($l < $size) {
  return $l;
}
  return -1;
}
function right($i, $size) {
  global $h;
  $r = 2 * $i + 2;
  if ($r < $size) {
  return $r;
}
  return -1;
}
function swap(&$h, $i, $j) {
  $arr = $h['arr'];
  $item_i = $arr[$i][0];
  $item_j = $arr[$j][0];
  $pm = $h['pos_map'];
  $pm[$item_i] = $j + 1;
  $pm[$item_j] = $i + 1;
  $h['pos_map'] = $pm;
  $tmp = $arr[$i];
  $arr[$i] = $arr[$j];
  $arr[$j] = $tmp;
  $h['arr'] = $arr;
}
function cmp($h, $i, $j) {
  $arr = $h['arr'];
  return $arr[$i][1] < $arr[$j][1];
}
function get_valid_parent($h, $i) {
  $vp = $i;
  $l = left($i, $h['size']);
  if ($l != 0 - 1 && cmp($h, $l, $vp) == false) {
  $vp = $l;
}
  $r = right($i, $h['size']);
  if ($r != 0 - 1 && cmp($h, $r, $vp) == false) {
  $vp = $r;
}
  return $vp;
}
function heapify_up(&$h, $index) {
  $idx = $index;
  $p = parent($idx);
  while ($p != 0 - 1 && cmp($h, $idx, $p) == false) {
  swap($h, $idx, $p);
  $idx = $p;
  $p = parent($p);
};
}
function heapify_down(&$h, $index) {
  $idx = $index;
  $vp = get_valid_parent($h, $idx);
  while ($vp != $idx) {
  swap($h, $idx, $vp);
  $idx = $vp;
  $vp = get_valid_parent($h, $idx);
};
}
function update_item(&$h, $item, $item_value) {
  $pm = $h['pos_map'];
  if ($pm[$item] == 0) {
  return;
}
  $index = $pm[$item] - 1;
  $arr = $h['arr'];
  $arr[$index] = [$item, $h['key']($item_value)];
  $h['arr'] = $arr;
  $h['pos_map'] = $pm;
  heapify_up($h, $index);
  heapify_down($h, $index);
}
function delete_item(&$h, $item) {
  $pm = $h['pos_map'];
  if ($pm[$item] == 0) {
  return;
}
  $index = $pm[$item] - 1;
  $pm[$item] = 0;
  $arr = $h['arr'];
  $last_index = $h['size'] - 1;
  if ($index != $last_index) {
  $arr[$index] = $arr[$last_index];
  $moved = $arr[$index][0];
  $pm[$moved] = $index + 1;
}
  $h['size'] = $h['size'] - 1;
  $h['arr'] = $arr;
  $h['pos_map'] = $pm;
  if ($h['size'] > $index) {
  heapify_up($h, $index);
  heapify_down($h, $index);
}
}
function insert_item(&$h, $item, $item_value) {
  $arr = $h['arr'];
  $arr_len = count($arr);
  if ($arr_len == $h['size']) {
  $arr = _append($arr, [$item, $h['key']($item_value)]);
} else {
  $arr[$h['size']] = [$item, $h['key']($item_value)];
}
  $pm = $h['pos_map'];
  $pm[$item] = $h['size'] + 1;
  $h['size'] = $h['size'] + 1;
  $h['arr'] = $arr;
  $h['pos_map'] = $pm;
  heapify_up($h, $h['size'] - 1);
}
function get_top($h) {
  $arr = $h['arr'];
  if ($h['size'] > 0) {
  return $arr[0];
}
  return [];
}
function extract_top(&$h) {
  $top = get_top($h);
  if (count($top) > 0) {
  delete_item($h, $top[0]);
}
  return $top;
}
function identity($x) {
  global $h;
  return $x;
}
function negate($x) {
  global $h;
  return 0 - $x;
}
$h = new_heap('identity');
insert_item($h, 5, 34);
insert_item($h, 6, 31);
insert_item($h, 7, 37);
echo rtrim(_str(get_top($h))), PHP_EOL;
echo rtrim(_str(extract_top($h))), PHP_EOL;
echo rtrim(_str(extract_top($h))), PHP_EOL;
echo rtrim(_str(extract_top($h))), PHP_EOL;
$h = new_heap('negate');
insert_item($h, 5, 34);
insert_item($h, 6, 31);
insert_item($h, 7, 37);
echo rtrim(_str(get_top($h))), PHP_EOL;
echo rtrim(_str(extract_top($h))), PHP_EOL;
echo rtrim(_str(extract_top($h))), PHP_EOL;
echo rtrim(_str(extract_top($h))), PHP_EOL;
insert_item($h, 8, 45);
insert_item($h, 9, 40);
insert_item($h, 10, 50);
echo rtrim(_str(get_top($h))), PHP_EOL;
update_item($h, 10, 30);
echo rtrim(_str(get_top($h))), PHP_EOL;
delete_item($h, 10);
echo rtrim(_str(get_top($h))), PHP_EOL;
