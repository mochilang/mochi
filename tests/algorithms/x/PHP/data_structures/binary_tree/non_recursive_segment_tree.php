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
function build($arr, $combine) {
  global $arr1, $st1, $arr2, $st2, $arr3, $st3, $arr4, $n4, $st4;
  $n = count($arr);
  $st = [];
  $i = 0;
  while ($i < 2 * $n) {
  $st = _append($st, 0);
  $i = $i + 1;
};
  $i = 0;
  while ($i < $n) {
  $st[$n + $i] = $arr[$i];
  $i = $i + 1;
};
  $i = $n - 1;
  while ($i > 0) {
  $st[$i] = $combine($st[$i * 2], $st[$i * 2 + 1]);
  $i = $i - 1;
};
  return $st;
}
function update(&$st, $n, $combine, $p, $v) {
  global $arr1, $st1, $arr2, $st2, $arr3, $st3, $arr4, $n4, $st4;
  $idx = $p + $n;
  $st[$idx] = $v;
  while ($idx > 1) {
  $idx = intval((_intdiv($idx, 2)));
  $st[$idx] = $combine($st[$idx * 2], $st[$idx * 2 + 1]);
};
}
function query($st, $n, $combine, $left, $right) {
  global $arr1, $st1, $arr2, $st2, $arr3, $st3, $arr4, $n4, $st4;
  $l = $left + $n;
  $r = $right + $n;
  $res = 0;
  $has = false;
  while ($l <= $r) {
  if ($l % 2 == 1) {
  if (!$has) {
  $res = $st[$l];
  $has = true;
} else {
  $res = $combine($res, $st[$l]);
};
  $l = $l + 1;
}
  if ($r % 2 == 0) {
  if (!$has) {
  $res = $st[$r];
  $has = true;
} else {
  $res = $combine($res, $st[$r]);
};
  $r = $r - 1;
}
  $l = intval((_intdiv($l, 2)));
  $r = intval((_intdiv($r, 2)));
};
  return $res;
}
function add($a, $b) {
  global $arr1, $st1, $arr2, $st2, $arr3, $st3, $arr4, $n4, $st4;
  return $a + $b;
}
function min_int($a, $b) {
  global $arr1, $st1, $arr2, $st2, $arr3, $st3, $arr4, $n4, $st4;
  if ($a < $b) {
  return $a;
} else {
  return $b;
}
}
function max_int($a, $b) {
  global $arr1, $st1, $arr2, $st2, $arr3, $st3, $arr4, $n4, $st4;
  if ($a > $b) {
  return $a;
} else {
  return $b;
}
}
$arr1 = [1, 2, 3];
$st1 = build($arr1, 'add');
echo rtrim(_str(query($st1, count($arr1), 'add', 0, 2))), PHP_EOL;
$arr2 = [3, 1, 2];
$st2 = build($arr2, 'min_int');
echo rtrim(_str(query($st2, count($arr2), 'min_int', 0, 2))), PHP_EOL;
$arr3 = [2, 3, 1];
$st3 = build($arr3, 'max_int');
echo rtrim(_str(query($st3, count($arr3), 'max_int', 0, 2))), PHP_EOL;
$arr4 = [1, 5, 7, -1, 6];
$n4 = count($arr4);
$st4 = build($arr4, 'add');
update($st4, $n4, 'add', 1, -1);
update($st4, $n4, 'add', 2, 3);
echo rtrim(_str(query($st4, $n4, 'add', 1, 2))), PHP_EOL;
echo rtrim(_str(query($st4, $n4, 'add', 1, 1))), PHP_EOL;
update($st4, $n4, 'add', 4, 1);
echo rtrim(_str(query($st4, $n4, 'add', 3, 4))), PHP_EOL;
