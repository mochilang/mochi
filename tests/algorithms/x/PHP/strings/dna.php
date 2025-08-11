<?php
ini_set('memory_limit', '-1');
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
function is_valid($strand) {
  $i = 0;
  while ($i < strlen($strand)) {
  $ch = substr($strand, $i, _iadd($i, 1) - $i);
  if ($ch != 'A' && $ch != 'T' && $ch != 'C' && $ch != 'G') {
  return false;
}
  $i = _iadd($i, 1);
};
  return true;
}
function dna($strand) {
  if (!is_valid($strand)) {
  echo rtrim('ValueError: Invalid Strand'), PHP_EOL;
  return '';
}
  $result = '';
  $i = 0;
  while ($i < strlen($strand)) {
  $ch = substr($strand, $i, _iadd($i, 1) - $i);
  if ($ch == 'A') {
  $result = $result . 'T';
} else {
  if ($ch == 'T') {
  $result = $result . 'A';
} else {
  if ($ch == 'C') {
  $result = $result . 'G';
} else {
  $result = $result . 'C';
};
};
}
  $i = _iadd($i, 1);
};
  return $result;
}
echo rtrim(dna('GCTA')), PHP_EOL;
echo rtrim(dna('ATGC')), PHP_EOL;
echo rtrim(dna('CTGA')), PHP_EOL;
echo rtrim(dna('GFGG')), PHP_EOL;
