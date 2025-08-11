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
$ETAOIN = 'ETAOINSHRDLCUMWFGYPBVKJXQZ';
$LETTERS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
function etaoin_index($letter) {
  global $ETAOIN, $LETTERS;
  $i = 0;
  while ($i < strlen($ETAOIN)) {
  if (substr($ETAOIN, $i, _iadd($i, 1) - $i) == $letter) {
  return $i;
}
  $i = _iadd($i, 1);
};
  return strlen($ETAOIN);
}
function get_letter_count($message) {
  global $ETAOIN, $LETTERS;
  $letter_count = [];
  $i = 0;
  while ($i < strlen($LETTERS)) {
  $c = substr($LETTERS, $i, _iadd($i, 1) - $i);
  $letter_count[$c] = 0;
  $i = _iadd($i, 1);
};
  $msg = strtoupper($message);
  $j = 0;
  while ($j < strlen($msg)) {
  $ch = substr($msg, $j, _iadd($j, 1) - $j);
  if (strpos($LETTERS, $ch) !== false) {
  $letter_count[$ch] = _iadd($letter_count[$ch], 1);
}
  $j = _iadd($j, 1);
};
  return $letter_count;
}
function get_frequency_order($message) {
  global $ETAOIN, $LETTERS;
  $letter_to_freq = get_letter_count($message);
  $max_freq = 0;
  $i = 0;
  while ($i < strlen($LETTERS)) {
  $letter = substr($LETTERS, $i, _iadd($i, 1) - $i);
  $f = $letter_to_freq[$letter];
  if ($f > $max_freq) {
  $max_freq = $f;
}
  $i = _iadd($i, 1);
};
  $result = '';
  $freq = $max_freq;
  while ($freq >= 0) {
  $group = [];
  $j = 0;
  while ($j < strlen($LETTERS)) {
  $letter = substr($LETTERS, $j, _iadd($j, 1) - $j);
  if ($letter_to_freq[$letter] == $freq) {
  $group = _append($group, $letter);
}
  $j = _iadd($j, 1);
};
  $g_len = count($group);
  $a = 0;
  while ($a < $g_len) {
  $b = 0;
  while ($b < _isub(_isub($g_len, $a), 1)) {
  $g1 = $group[$b];
  $g2 = $group[_iadd($b, 1)];
  $idx1 = etaoin_index($g1);
  $idx2 = etaoin_index($g2);
  if ($idx1 < $idx2) {
  $tmp = $group[$b];
  $group[$b] = $group[_iadd($b, 1)];
  $group[_iadd($b, 1)] = $tmp;
}
  $b = _iadd($b, 1);
};
  $a = _iadd($a, 1);
};
  $g = 0;
  while ($g < count($group)) {
  $result = $result . $group[$g];
  $g = _iadd($g, 1);
};
  $freq = _isub($freq, 1);
};
  return $result;
}
function english_freq_match_score($message) {
  global $ETAOIN, $LETTERS;
  $freq_order = get_frequency_order($message);
  $top = substr($freq_order, 0, 6);
  $bottom = substr($freq_order, _isub(strlen($freq_order), 6), strlen($freq_order) - _isub(strlen($freq_order), 6));
  $score = 0;
  $i = 0;
  while ($i < 6) {
  $c = substr($ETAOIN, $i, _iadd($i, 1) - $i);
  if (strpos($top, $c) !== false) {
  $score = _iadd($score, 1);
}
  $i = _iadd($i, 1);
};
  $j = _isub(strlen($ETAOIN), 6);
  while ($j < strlen($ETAOIN)) {
  $c = substr($ETAOIN, $j, _iadd($j, 1) - $j);
  if (strpos($bottom, $c) !== false) {
  $score = _iadd($score, 1);
}
  $j = _iadd($j, 1);
};
  return $score;
}
function main() {
  global $ETAOIN, $LETTERS;
  echo rtrim(get_frequency_order('Hello World')), PHP_EOL;
  echo rtrim(json_encode(english_freq_match_score('Hello World'), 1344)), PHP_EOL;
}
main();
