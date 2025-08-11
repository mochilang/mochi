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
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
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
function mochi_xor($a, $b) {
  global $ascii_chars, $LOWERCASE_INTS, $i, $COMMON_WORDS, $ciphertext;
  $res = 0;
  $bit = 1;
  $x = $a;
  $y = $b;
  while ($x > 0 || $y > 0) {
  $abit = _imod($x, 2);
  $bbit = _imod($y, 2);
  if ($abit != $bbit) {
  $res = _iadd($res, $bit);
}
  $x = _intdiv($x, 2);
  $y = _intdiv($y, 2);
  $bit = _imul($bit, 2);
};
  return $res;
}
$ascii_chars = ' !"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~';
function mochi_chr($code) {
  global $ascii_chars, $LOWERCASE_INTS, $i, $COMMON_WORDS, $ciphertext;
  if ($code == 10) {
  return '
';
}
  if ($code == 13) {
  return '';
}
  if ($code == 9) {
  return '	';
}
  if ($code >= 32 && $code < 127) {
  return substr($ascii_chars, _isub($code, 32), _isub($code, 31) - _isub($code, 32));
}
  return '';
}
function mochi_ord($ch) {
  global $ascii_chars, $LOWERCASE_INTS, $COMMON_WORDS, $ciphertext;
  if ($ch == '
') {
  return 10;
}
  if ($ch == '') {
  return 13;
}
  if ($ch == '	') {
  return 9;
}
  $i = 0;
  while ($i < strlen($ascii_chars)) {
  if (substr($ascii_chars, $i, _iadd($i, 1) - $i) == $ch) {
  return _iadd(32, $i);
}
  $i = _iadd($i, 1);
};
  return 0;
}
function is_valid_ascii($code) {
  global $ascii_chars, $LOWERCASE_INTS, $i, $COMMON_WORDS, $ciphertext;
  if ($code >= 32 && $code <= 126) {
  return true;
}
  if ($code == 9 || $code == 10 || $code == 13) {
  return true;
}
  return false;
}
$LOWERCASE_INTS = [];
$i = 97;
while ($i <= 122) {
  $LOWERCASE_INTS = _append($LOWERCASE_INTS, $i);
  $i = _iadd($i, 1);
}
$COMMON_WORDS = ['the', 'be', 'to', 'of', 'and', 'in', 'that', 'have'];
function try_key($ciphertext, $key) {
  global $ascii_chars, $LOWERCASE_INTS, $COMMON_WORDS;
  $decoded = '';
  $i = 0;
  $klen = count($key);
  while ($i < count($ciphertext)) {
  $decodedchar = mochi_xor($ciphertext[$i], $key[_imod($i, $klen)]);
  if (!is_valid_ascii($decodedchar)) {
  return null;
}
  $decoded = $decoded . mochi_chr($decodedchar);
  $i = _iadd($i, 1);
};
  return $decoded;
}
function filter_valid_chars($ciphertext) {
  global $ascii_chars, $LOWERCASE_INTS, $COMMON_WORDS;
  $possibles = [];
  $i = 0;
  while ($i < count($LOWERCASE_INTS)) {
  $j = 0;
  while ($j < count($LOWERCASE_INTS)) {
  $k = 0;
  while ($k < count($LOWERCASE_INTS)) {
  $key = [$LOWERCASE_INTS[$i], $LOWERCASE_INTS[$j], $LOWERCASE_INTS[$k]];
  $decoded = try_key($ciphertext, $key);
  if ($decoded != null) {
  $possibles = _append($possibles, $decoded);
}
  $k = _iadd($k, 1);
};
  $j = _iadd($j, 1);
};
  $i = _iadd($i, 1);
};
  return $possibles;
}
function contains($s, $sub) {
  global $ascii_chars, $LOWERCASE_INTS, $COMMON_WORDS, $ciphertext;
  $n = strlen($s);
  $m = strlen($sub);
  if ($m == 0) {
  return true;
}
  $i = 0;
  while ($i <= _isub($n, $m)) {
  $j = 0;
  $is_match = true;
  while ($j < $m) {
  if (substr($s, _iadd($i, $j), _iadd($i, $j) + 1 - _iadd($i, $j)) != substr($sub, $j, $j + 1 - $j)) {
  $is_match = false;
  break;
}
  $j = _iadd($j, 1);
};
  if ($is_match) {
  return true;
}
  $i = _iadd($i, 1);
};
  return false;
}
function filter_common_word($possibles, $common_word) {
  global $ascii_chars, $LOWERCASE_INTS, $COMMON_WORDS, $ciphertext;
  $res = [];
  $i = 0;
  while ($i < count($possibles)) {
  $p = $possibles[$i];
  if (contains(strtolower($p), $common_word)) {
  $res = _append($res, $p);
}
  $i = _iadd($i, 1);
};
  return $res;
}
function solution($ciphertext) {
  global $ascii_chars, $LOWERCASE_INTS, $COMMON_WORDS;
  $possibles = filter_valid_chars($ciphertext);
  $i = 0;
  while ($i < count($COMMON_WORDS)) {
  $word = $COMMON_WORDS[$i];
  $possibles = filter_common_word($possibles, $word);
  if (count($possibles) == 1) {
  break;
}
  $i = _iadd($i, 1);
};
  $decoded_text = $possibles[0];
  $sum = 0;
  $j = 0;
  while ($j < strlen($decoded_text)) {
  $sum = _iadd($sum, mochi_ord(substr($decoded_text, $j, _iadd($j, 1) - $j)));
  $j = _iadd($j, 1);
};
  return $sum;
}
$ciphertext = [17, 6, 1, 69, 12, 1, 69, 26, 11, 69, 1, 2, 69, 15, 10, 1, 78, 13, 11, 78, 16, 13, 15, 16, 69, 6, 5, 19, 11];
echo rtrim(_str(solution($ciphertext))), PHP_EOL;
