<?php
ini_set('memory_limit', '-1');
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
function match_in_pattern($pat, $ch) {
  $i = _isub(strlen($pat), 1);
  while ($i >= 0) {
  if (substr($pat, $i, _iadd($i, 1) - $i) == $ch) {
  return $i;
}
  $i = _isub($i, 1);
};
  return -1;
}
function mismatch_in_text($text, $pat, $current_pos) {
  $i = _isub(strlen($pat), 1);
  while ($i >= 0) {
  if (substr($pat, $i, _iadd($i, 1) - $i) != substr($text, _iadd($current_pos, $i), _iadd(_iadd($current_pos, $i), 1) - _iadd($current_pos, $i))) {
  return _iadd($current_pos, $i);
}
  $i = _isub($i, 1);
};
  return -1;
}
function bad_character_heuristic($text, $pat) {
  $textLen = strlen($text);
  $patLen = strlen($pat);
  $positions = [];
  $i = 0;
  while ($i <= _isub($textLen, $patLen)) {
  $mismatch_index = mismatch_in_text($text, $pat, $i);
  if ($mismatch_index < 0) {
  $positions = _append($positions, $i);
  $i = _iadd($i, 1);
} else {
  $ch = substr($text, $mismatch_index, _iadd($mismatch_index, 1) - $mismatch_index);
  $match_index = match_in_pattern($pat, $ch);
  if ($match_index < 0) {
  $i = _iadd($mismatch_index, 1);
} else {
  $i = _isub($mismatch_index, $match_index);
};
}
};
  return $positions;
}
