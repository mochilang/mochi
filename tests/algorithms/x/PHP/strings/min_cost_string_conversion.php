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
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}
function string_to_chars($s) {
  $chars = [];
  $i = 0;
  while ($i < strlen($s)) {
  $chars = _append($chars, $s[$i]);
  $i = _iadd($i, 1);
};
  return $chars;
}
function join_chars($chars) {
  $res = '';
  $i = 0;
  while ($i < count($chars)) {
  $res = $res . $chars[$i];
  $i = _iadd($i, 1);
};
  return $res;
}
function insert_at($chars, $index, $ch) {
  $res = [];
  $i = 0;
  while ($i < $index) {
  $res = _append($res, $chars[$i]);
  $i = _iadd($i, 1);
};
  $res = _append($res, $ch);
  while ($i < count($chars)) {
  $res = _append($res, $chars[$i]);
  $i = _iadd($i, 1);
};
  return $res;
}
function remove_at($chars, $index) {
  $res = [];
  $i = 0;
  while ($i < count($chars)) {
  if ($i != $index) {
  $res = _append($res, $chars[$i]);
}
  $i = _iadd($i, 1);
};
  return $res;
}
function make_matrix_int($rows, $cols, $init) {
  $matrix = [];
  for ($_ = 0; $_ < $rows; $_++) {
  $row = [];
  for ($_2 = 0; $_2 < $cols; $_2++) {
  $row = _append($row, $init);
};
  $matrix = _append($matrix, $row);
};
  return $matrix;
}
function make_matrix_string($rows, $cols, $init) {
  $matrix = [];
  for ($_ = 0; $_ < $rows; $_++) {
  $row = [];
  for ($_2 = 0; $_2 < $cols; $_2++) {
  $row = _append($row, $init);
};
  $matrix = _append($matrix, $row);
};
  return $matrix;
}
function compute_transform_tables($source_string, $destination_string, $copy_cost, $replace_cost, $delete_cost, $insert_cost) {
  $source_seq = string_to_chars($source_string);
  $dest_seq = string_to_chars($destination_string);
  $m = count($source_seq);
  $n = count($dest_seq);
  $costs = make_matrix_int(_iadd($m, 1), _iadd($n, 1), 0);
  $ops = make_matrix_string(_iadd($m, 1), _iadd($n, 1), '0');
  $i = 1;
  while ($i <= $m) {
  $costs[$i][0] = _imul($i, $delete_cost);
  $ops[$i][0] = 'D' . $source_seq[_isub($i, 1)];
  $i = _iadd($i, 1);
};
  $j = 1;
  while ($j <= $n) {
  $costs[0][$j] = _imul($j, $insert_cost);
  $ops[0][$j] = 'I' . $dest_seq[_isub($j, 1)];
  $j = _iadd($j, 1);
};
  $i = 1;
  while ($i <= $m) {
  $j = 1;
  while ($j <= $n) {
  if ($source_seq[_isub($i, 1)] == $dest_seq[_isub($j, 1)]) {
  $costs[$i][$j] = _iadd($costs[_isub($i, 1)][_isub($j, 1)], $copy_cost);
  $ops[$i][$j] = 'C' . $source_seq[_isub($i, 1)];
} else {
  $costs[$i][$j] = _iadd($costs[_isub($i, 1)][_isub($j, 1)], $replace_cost);
  $ops[$i][$j] = 'R' . $source_seq[_isub($i, 1)] . $dest_seq[_isub($j, 1)];
}
  if (_iadd($costs[_isub($i, 1)][$j], $delete_cost) < $costs[$i][$j]) {
  $costs[$i][$j] = _iadd($costs[_isub($i, 1)][$j], $delete_cost);
  $ops[$i][$j] = 'D' . $source_seq[_isub($i, 1)];
}
  if (_iadd($costs[$i][_isub($j, 1)], $insert_cost) < $costs[$i][$j]) {
  $costs[$i][$j] = _iadd($costs[$i][_isub($j, 1)], $insert_cost);
  $ops[$i][$j] = 'I' . $dest_seq[_isub($j, 1)];
}
  $j = _iadd($j, 1);
};
  $i = _iadd($i, 1);
};
  return ['costs' => $costs, 'ops' => $ops];
}
function assemble_transformation($ops, $i, $j) {
  if ($i == 0 && $j == 0) {
  return [];
}
  $op = $ops[$i][$j];
  $kind = substr($op, 0, 1);
  if ($kind == 'C' || $kind == 'R') {
  $seq = assemble_transformation($ops, _isub($i, 1), _isub($j, 1));
  $seq = _append($seq, $op);
  return $seq;
} else {
  if ($kind == 'D') {
  $seq = assemble_transformation($ops, _isub($i, 1), $j);
  $seq = _append($seq, $op);
  return $seq;
} else {
  $seq = assemble_transformation($ops, $i, _isub($j, 1));
  $seq = _append($seq, $op);
  return $seq;
};
}
}
function main() {
  $copy_cost = -1;
  $replace_cost = 1;
  $delete_cost = 2;
  $insert_cost = 2;
  $src = 'Python';
  $dst = 'Algorithms';
  $tables = compute_transform_tables($src, $dst, $copy_cost, $replace_cost, $delete_cost, $insert_cost);
  $operations = $tables['ops'];
  $m = count($operations);
  $n = count($operations[0]);
  $sequence = assemble_transformation($operations, _isub($m, 1), _isub($n, 1));
  $string_list = string_to_chars($src);
  $idx = 0;
  $cost = 0;
  $k = 0;
  while ($k < count($sequence)) {
  echo rtrim(join_chars($string_list)), PHP_EOL;
  $op = $sequence[$k];
  $kind = substr($op, 0, 1);
  if ($kind == 'C') {
  $cost = _iadd($cost, $copy_cost);
} else {
  if ($kind == 'R') {
  $string_list[$idx] = substr($op, 2, 3 - 2);
  $cost = _iadd($cost, $replace_cost);
} else {
  if ($kind == 'D') {
  $string_list = remove_at($string_list, $idx);
  $cost = _iadd($cost, $delete_cost);
} else {
  $string_list = insert_at($string_list, $idx, substr($op, 1, 2 - 1));
  $cost = _iadd($cost, $insert_cost);
};
};
}
  $idx = _iadd($idx, 1);
  $k = _iadd($k, 1);
};
  echo rtrim(join_chars($string_list)), PHP_EOL;
  echo rtrim('Cost: ' . _str($cost)), PHP_EOL;
}
main();
