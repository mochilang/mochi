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
function parse_int($s) {
  $value = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, _iadd($i, 1) - $i);
  if ($ch == ',') {
  $i = _iadd($i, 1);
  continue;
}
  $value = _iadd(_imul($value, 10), (intval($ch)));
  $i = _iadd($i, 1);
};
  return $value;
}
function find($haystack, $needle, $start) {
  $nlen = strlen($needle);
  $i = $start;
  while ($i <= _isub(strlen($haystack), $nlen)) {
  $j = 0;
  $matched = true;
  while ($j < $nlen) {
  if (substr($haystack, _iadd($i, $j), _iadd(_iadd($i, $j), 1) - _iadd($i, $j)) != substr($needle, $j, _iadd($j, 1) - $j)) {
  $matched = false;
  break;
}
  $j = _iadd($j, 1);
};
  if ($matched) {
  return $i;
}
  $i = _iadd($i, 1);
};
  return _isub(0, 1);
}
function extract_numbers($html) {
  $nums = [];
  $pos = 0;
  $start_tag = '<span>';
  $end_tag = '</span>';
  while (true) {
  $s = find($html, $start_tag, $pos);
  if ($s == _isub(0, 1)) {
  break;
}
  $content_start = _iadd($s, strlen($start_tag));
  $e = find($html, $end_tag, $content_start);
  if ($e == _isub(0, 1)) {
  break;
}
  $num_str = substr($html, $content_start, $e - $content_start);
  $nums = _append($nums, parse_int($num_str));
  $pos = _iadd($e, strlen($end_tag));
};
  return $nums;
}
function covid_stats($html) {
  $nums = extract_numbers($html);
  return ['cases' => $nums[0], 'deaths' => $nums[1], 'recovered' => $nums[2]];
}
function main() {
  $sample_html = '<div class="maincounter-number"><span>123456</span></div>' . '<div class="maincounter-number"><span>7890</span></div>' . '<div class="maincounter-number"><span>101112</span></div>';
  $stats = covid_stats($sample_html);
  echo rtrim('Total COVID-19 cases in the world: ' . _str($stats['cases'])), PHP_EOL;
  echo rtrim('Total deaths due to COVID-19 in the world: ' . _str($stats['deaths'])), PHP_EOL;
  echo rtrim('Total COVID-19 patients recovered in the world: ' . _str($stats['recovered'])), PHP_EOL;
}
main();
