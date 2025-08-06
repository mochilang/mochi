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
function tail($xs) {
  $res = [];
  $i = 1;
  while ($i < count($xs)) {
  $res = _append($res, $xs[$i]);
  $i = $i + 1;
};
  return $res;
}
function rotate_left($xs) {
  if (count($xs) == 0) {
  return $xs;
}
  $res = [];
  $i = 1;
  while ($i < count($xs)) {
  $res = _append($res, $xs[$i]);
  $i = $i + 1;
};
  $res = _append($res, $xs[0]);
  return $res;
}
function permute_recursive($nums) {
  if (count($nums) == 0) {
  $base = [];
  return _append($base, []);
}
  $result = [];
  $current = $nums;
  $count = 0;
  while ($count < count($nums)) {
  $n = $current[0];
  $rest = tail($current);
  $perms = permute_recursive($rest);
  $j = 0;
  while ($j < count($perms)) {
  $perm = _append($perms[$j], $n);
  $result = _append($result, $perm);
  $j = $j + 1;
};
  $current = rotate_left($current);
  $count = $count + 1;
};
  return $result;
}
function swap($xs, $i, $j) {
  $res = [];
  $k = 0;
  while ($k < count($xs)) {
  if ($k == $i) {
  $res = _append($res, $xs[$j]);
} else {
  if ($k == $j) {
  $res = _append($res, $xs[$i]);
} else {
  $res = _append($res, $xs[$k]);
};
}
  $k = $k + 1;
};
  return $res;
}
function permute_backtrack_helper($nums, $start, $output) {
  if ($start == count($nums) - 1) {
  return _append($output, $nums);
}
  $i = $start;
  $res = $output;
  while ($i < count($nums)) {
  $swapped = swap($nums, $start, $i);
  $res = permute_backtrack_helper($swapped, $start + 1, $res);
  $i = $i + 1;
};
  return $res;
}
function permute_backtrack($nums) {
  $output = [];
  return permute_backtrack_helper($nums, 0, $output);
}
echo rtrim(_str(permute_recursive([1, 2, 3]))), PHP_EOL;
echo rtrim(_str(permute_backtrack([1, 2, 3]))), PHP_EOL;
