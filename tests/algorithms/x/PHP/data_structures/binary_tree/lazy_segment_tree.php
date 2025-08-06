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
function init_int_array($n) {
  global $NEG_INF, $A, $segment_tree, $lazy, $flag;
  $arr = [];
  $i = 0;
  while ($i < 4 * $n + 5) {
  $arr = _append($arr, 0);
  $i = $i + 1;
};
  return $arr;
}
function init_bool_array($n) {
  global $NEG_INF, $A, $segment_tree, $lazy, $flag;
  $arr = [];
  $i = 0;
  while ($i < 4 * $n + 5) {
  $arr = _append($arr, false);
  $i = $i + 1;
};
  return $arr;
}
function left($idx) {
  global $NEG_INF, $A, $n, $segment_tree, $lazy, $flag;
  return $idx * 2;
}
function right($idx) {
  global $NEG_INF, $A, $n, $segment_tree, $lazy, $flag;
  return $idx * 2 + 1;
}
function build(&$segment_tree, $idx, $l, $r, $a) {
  global $NEG_INF, $A, $n, $lazy, $flag;
  if ($l == $r) {
  $segment_tree[$idx] = $a[$l - 1];
} else {
  $mid = _intdiv(($l + $r), 2);
  build($segment_tree, left($idx), $l, $mid, $a);
  build($segment_tree, right($idx), $mid + 1, $r, $a);
  $lv = $segment_tree[left($idx)];
  $rv = $segment_tree[right($idx)];
  if ($lv > $rv) {
  $segment_tree[$idx] = $lv;
} else {
  $segment_tree[$idx] = $rv;
};
}
}
function update(&$segment_tree, &$lazy, &$flag, $idx, $l, $r, $a, $b, $val) {
  global $NEG_INF, $A, $n;
  if ($flag[$idx]) {
  $segment_tree[$idx] = $lazy[$idx];
  $flag[$idx] = false;
  if ($l != $r) {
  $lazy[left($idx)] = $lazy[$idx];
  $lazy[right($idx)] = $lazy[$idx];
  $flag[left($idx)] = true;
  $flag[right($idx)] = true;
};
}
  if ($r < $a || $l > $b) {
  return;
}
  if ($l >= $a && $r <= $b) {
  $segment_tree[$idx] = $val;
  if ($l != $r) {
  $lazy[left($idx)] = $val;
  $lazy[right($idx)] = $val;
  $flag[left($idx)] = true;
  $flag[right($idx)] = true;
};
  return;
}
  $mid = _intdiv(($l + $r), 2);
  update($segment_tree, $lazy, $flag, left($idx), $l, $mid, $a, $b, $val);
  update($segment_tree, $lazy, $flag, right($idx), $mid + 1, $r, $a, $b, $val);
  $lv = $segment_tree[left($idx)];
  $rv = $segment_tree[right($idx)];
  if ($lv > $rv) {
  $segment_tree[$idx] = $lv;
} else {
  $segment_tree[$idx] = $rv;
}
}
$NEG_INF = -1000000000;
function query(&$segment_tree, &$lazy, &$flag, $idx, $l, $r, $a, $b) {
  global $NEG_INF, $A, $n;
  if ($flag[$idx]) {
  $segment_tree[$idx] = $lazy[$idx];
  $flag[$idx] = false;
  if ($l != $r) {
  $lazy[left($idx)] = $lazy[$idx];
  $lazy[right($idx)] = $lazy[$idx];
  $flag[left($idx)] = true;
  $flag[right($idx)] = true;
};
}
  if ($r < $a || $l > $b) {
  return $NEG_INF;
}
  if ($l >= $a && $r <= $b) {
  return $segment_tree[$idx];
}
  $mid = _intdiv(($l + $r), 2);
  $q1 = query($segment_tree, $lazy, $flag, left($idx), $l, $mid, $a, $b);
  $q2 = query($segment_tree, $lazy, $flag, right($idx), $mid + 1, $r, $a, $b);
  if ($q1 > $q2) {
  return $q1;
} else {
  return $q2;
}
}
function segtree_to_string(&$segment_tree, &$lazy, &$flag, $n) {
  global $NEG_INF, $A;
  $res = '[';
  $i = 1;
  while ($i <= $n) {
  $v = query($segment_tree, $lazy, $flag, 1, 1, $n, $i, $i);
  $res = $res . _str($v);
  if ($i < $n) {
  $res = $res . ', ';
}
  $i = $i + 1;
};
  $res = $res . ']';
  return $res;
}
$A = [1, 2, -4, 7, 3, -5, 6, 11, -20, 9, 14, 15, 5, 2, -8];
$n = 15;
$segment_tree = init_int_array($n);
$lazy = init_int_array($n);
$flag = init_bool_array($n);
build($segment_tree, 1, 1, $n, $A);
echo rtrim(json_encode(query($segment_tree, $lazy, $flag, 1, 1, $n, 4, 6), 1344)), PHP_EOL;
echo rtrim(json_encode(query($segment_tree, $lazy, $flag, 1, 1, $n, 7, 11), 1344)), PHP_EOL;
echo rtrim(json_encode(query($segment_tree, $lazy, $flag, 1, 1, $n, 7, 12), 1344)), PHP_EOL;
update($segment_tree, $lazy, $flag, 1, 1, $n, 1, 3, 111);
echo rtrim(json_encode(query($segment_tree, $lazy, $flag, 1, 1, $n, 1, 15), 1344)), PHP_EOL;
update($segment_tree, $lazy, $flag, 1, 1, $n, 7, 8, 235);
echo rtrim(segtree_to_string($segment_tree, $lazy, $flag, $n)), PHP_EOL;
