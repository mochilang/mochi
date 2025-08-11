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
$DIGITS = '0123456789';
function is_digit($ch) {
  global $DIGITS, $params;
  $i = 0;
  while ($i < strlen($DIGITS)) {
  if (substr($DIGITS, $i, $i + 1 - $i) == $ch) {
  return true;
}
  $i = _iadd($i, 1);
};
  return false;
}
function find_substring($haystack, $needle) {
  global $DIGITS, $params;
  $i = 0;
  while ($i <= strlen($haystack) - strlen($needle)) {
  $j = 0;
  while ($j < strlen($needle)) {
  if (substr($haystack, _iadd($i, $j), _iadd($i, $j) + 1 - _iadd($i, $j)) != substr($needle, $j, $j + 1 - $j)) {
  break;
}
  $j = _iadd($j, 1);
};
  if ($j == strlen($needle)) {
  return $i;
}
  $i = _iadd($i, 1);
};
  return -1;
}
function extract_citation($html) {
  global $DIGITS, $params;
  $marker = 'Cited by ';
  $idx = find_substring($html, $marker);
  if ($idx < 0) {
  return '';
}
  $pos = _iadd($idx, strlen($marker));
  $result = '';
  while ($pos < strlen($html)) {
  $ch = substr($html, $pos, $pos + 1 - $pos);
  if (!is_digit($ch)) {
  break;
}
  $result = $result . $ch;
  $pos = _iadd($pos, 1);
};
  return $result;
}
function get_citation($base_url, $params) {
  global $DIGITS;
  $html = '<div class="gs_ri"><div class="gs_fl"><a>Cited by 123</a></div></div>';
  return extract_citation($html);
}
if ($__name__ == '__main__') {
  $params = ['title' => 'Precisely geometry controlled microsupercapacitors for ultrahigh areal capacitance, volumetric capacitance, and energy density', 'journal' => 'Chem. Mater.', 'volume' => '30', 'pages' => '3979-3990', 'year' => '2018', 'hl' => 'en'];
  echo rtrim(get_citation('https://scholar.google.com/scholar_lookup', $params)), PHP_EOL;
}
