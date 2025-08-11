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
function min_int($a, $b) {
  if ($a < $b) {
  return $a;
} else {
  return $b;
}
}
function max_int($a, $b) {
  if ($a > $b) {
  return $a;
} else {
  return $b;
}
}
function repeat_bool($n, $value) {
  $res = [];
  $i = 0;
  while ($i < $n) {
  $res = _append($res, $value);
  $i = _iadd($i, 1);
};
  return $res;
}
function set_bool($xs, $idx, $value) {
  $res = [];
  $i = 0;
  while ($i < count($xs)) {
  if ($i == $idx) {
  $res = _append($res, $value);
} else {
  $res = _append($res, $xs[$i]);
}
  $i = _iadd($i, 1);
};
  return $res;
}
function jaro_winkler($s1, $s2) {
  $len1 = strlen($s1);
  $len2 = strlen($s2);
  $limit = _idiv(min_int($len1, $len2), 2);
  $match1 = repeat_bool($len1, false);
  $match2 = repeat_bool($len2, false);
  $matches = 0;
  $i = 0;
  while ($i < $len1) {
  $start = max_int(0, _isub($i, $limit));
  $end = min_int(_iadd(_iadd($i, $limit), 1), $len2);
  $j = $start;
  while ($j < $end) {
  if (!$match2[$j] && substr($s1, $i, _iadd($i, 1) - $i) == substr($s2, $j, _iadd($j, 1) - $j)) {
  $match1 = set_bool($match1, $i, true);
  $match2 = set_bool($match2, $j, true);
  $matches = _iadd($matches, 1);
  break;
}
  $j = _iadd($j, 1);
};
  $i = _iadd($i, 1);
};
  if ($matches == 0) {
  return 0.0;
}
  $transpositions = 0;
  $k = 0;
  $i = 0;
  while ($i < $len1) {
  if ($match1[$i]) {
  while (!$match2[$k]) {
  $k = _iadd($k, 1);
};
  if (substr($s1, $i, _iadd($i, 1) - $i) != substr($s2, $k, _iadd($k, 1) - $k)) {
  $transpositions = _iadd($transpositions, 1);
};
  $k = _iadd($k, 1);
}
  $i = _iadd($i, 1);
};
  $m = floatval($matches);
  $jaro = (($m / (floatval($len1))) + ($m / (floatval($len2))) + (($m - (floatval($transpositions)) / 2.0) / $m)) / 3.0;
  $prefix_len = 0;
  $i = 0;
  while ($i < 4 && $i < $len1 && $i < $len2) {
  if (substr($s1, $i, _iadd($i, 1) - $i) == substr($s2, $i, _iadd($i, 1) - $i)) {
  $prefix_len = _iadd($prefix_len, 1);
} else {
  break;
}
  $i = _iadd($i, 1);
};
  return $jaro + 0.1 * (floatval($prefix_len)) * (1.0 - $jaro);
}
echo rtrim(_str(jaro_winkler('hello', 'world'))), PHP_EOL;
