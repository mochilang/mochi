<?php
ini_set('memory_limit', '-1');
function repeat($s, $n) {
    return str_repeat($s, intval($n));
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
function split($s, $sep) {
  global $sample;
  $parts = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($s)) {
  if (strlen($sep) > 0 && _iadd($i, strlen($sep)) <= strlen($s) && substr($s, $i, _iadd($i, strlen($sep)) - $i) == $sep) {
  $parts = _append($parts, $cur);
  $cur = '';
  $i = _iadd($i, strlen($sep));
} else {
  $cur = $cur . substr($s, $i, _iadd($i, 1) - $i);
  $i = _iadd($i, 1);
}
};
  $parts = _append($parts, $cur);
  return $parts;
}
function mochi_join($xs, $sep) {
  global $sample;
  $res = '';
  $i = 0;
  while ($i < count($xs)) {
  if ($i > 0) {
  $res = $res . $sep;
}
  $res = $res . $xs[$i];
  $i = _iadd($i, 1);
};
  return $res;
}
function mochi_repeat($s, $n) {
  global $sample;
  $out = '';
  $i = 0;
  while ($i < $n) {
  $out = $out . $s;
  $i = _iadd($i, 1);
};
  return $out;
}
function replace_char($s, $old, $new) {
  global $sample;
  $out = '';
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, _iadd($i, 1) - $i);
  if ($c == $old) {
  $out = $out . $new;
} else {
  $out = $out . $c;
}
  $i = _iadd($i, 1);
};
  return $out;
}
function contains($s, $sub) {
  global $sample;
  if (strlen($sub) == 0) {
  return true;
}
  $i = 0;
  while (_iadd($i, strlen($sub)) <= strlen($s)) {
  if (substr($s, $i, _iadd($i, strlen($sub)) - $i) == $sub) {
  return true;
}
  $i = _iadd($i, 1);
};
  return false;
}
function file_extension($name) {
  global $sample;
  $i = _isub(strlen($name), 1);
  while ($i >= 0) {
  if (substr($name, $i, _iadd($i, 1) - $i) == '.') {
  return substr($name, $i);
}
  $i = _isub($i, 1);
};
  return '';
}
function remove_extension($name) {
  global $sample;
  $i = _isub(strlen($name), 1);
  while ($i >= 0) {
  if (substr($name, $i, _iadd($i, 1) - $i) == '.') {
  return substr($name, 0, $i);
}
  $i = _isub($i, 1);
};
  return $name;
}
function title_case($s) {
  global $sample;
  $out = '';
  $cap = true;
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, _iadd($i, 1) - $i);
  if ($c == ' ') {
  $out = $out . $c;
  $cap = true;
} else {
  if ($cap) {
  $out = $out . strtoupper($c);
  $cap = false;
} else {
  $out = $out . strtolower($c);
};
}
  $i = _iadd($i, 1);
};
  return $out;
}
function count_char($s, $ch) {
  global $sample;
  $cnt = 0;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, _iadd($i, 1) - $i) == $ch) {
  $cnt = _iadd($cnt, 1);
}
  $i = _iadd($i, 1);
};
  return $cnt;
}
function md_prefix($level) {
  global $sample;
  if ($level == 0) {
  return '
##';
}
  return repeat('  ', $level) . '*';
}
function print_path($old_path, $new_path) {
  global $sample;
  $old_parts = split($old_path, '/');
  $new_parts = split($new_path, '/');
  $i = 0;
  while ($i < count($new_parts)) {
  if (($i >= count($old_parts) || $old_parts[$i] != $new_parts[$i]) && $new_parts[$i] != '') {
  $title = title_case(replace_char($new_parts[$i], '_', ' '));
  echo rtrim(md_prefix($i) . ' ' . $title), PHP_EOL;
}
  $i = _iadd($i, 1);
};
  return $new_path;
}
function sort_strings($xs) {
  global $sample;
  $arr = $xs;
  $i = 0;
  while ($i < count($arr)) {
  $min_idx = $i;
  $j = _iadd($i, 1);
  while ($j < count($arr)) {
  if ($arr[$j] < $arr[$min_idx]) {
  $min_idx = $j;
}
  $j = _iadd($j, 1);
};
  $tmp = $arr[$i];
  $arr[$i] = $arr[$min_idx];
  $arr[$min_idx] = $tmp;
  $i = _iadd($i, 1);
};
  return $arr;
}
function good_file_paths($paths) {
  global $sample;
  $res = [];
  foreach ($paths as $p) {
  $parts = split($p, '/');
  $skip = false;
  $k = 0;
  while ($k < _isub(count($parts), 1)) {
  $part = $parts[$k];
  if ($part == 'scripts' || substr($part, 0, 1) == '.' || substr($part, 0, 1) == '_' || contains($part, 'venv')) {
  $skip = true;
}
  $k = _iadd($k, 1);
};
  if ($skip) {
  continue;
}
  $filename = $parts[_isub(count($parts), 1)];
  if ($filename == '__init__.py') {
  continue;
}
  $ext = file_extension($filename);
  if ($ext == '.py' || $ext == '.ipynb') {
  $res = _append($res, $p);
}
};
  return $res;
}
function print_directory_md($paths) {
  global $sample;
  $files = sort_strings(good_file_paths($paths));
  $old_path = '';
  $i = 0;
  while ($i < count($files)) {
  $fp = $files[$i];
  $parts = split($fp, '/');
  $filename = $parts[_isub(count($parts), 1)];
  $filepath = '';
  if (count($parts) > 1) {
  $filepath = mochi_join(array_slice($parts, 0, _isub(count($parts), 1)), '/');
}
  if ($filepath != $old_path) {
  $old_path = print_path($old_path, $filepath);
}
  $indent = 0;
  if (strlen($filepath) > 0) {
  $indent = _iadd(count_char($filepath, '/'), 1);
}
  $url = replace_char($fp, ' ', '%20');
  $name = title_case(replace_char(remove_extension($filename), '_', ' '));
  echo rtrim(md_prefix($indent) . ' [' . $name . '](' . $url . ')'), PHP_EOL;
  $i = _iadd($i, 1);
};
}
$sample = ['data_structures/linked_list.py', 'data_structures/binary_tree.py', 'math/number_theory/prime_check.py', 'math/number_theory/greatest_common_divisor.ipynb'];
print_directory_md($sample);
