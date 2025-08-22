<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
$_dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/project_euler/problem_042";
function _read_file($path) {
    $p = $path;
    if (!file_exists($p) && isset($_dataDir)) {
        $p = $_dataDir . DIRECTORY_SEPARATOR . $path;
    }
    $data = @file_get_contents($p);
    if ($data === false) return '';
    return $data;
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
    return intdiv(intval($a), intval($b));
}
function triangular_numbers($limit) {
  $res = [];
  $n = 1;
  while ($n <= $limit) {
  $res = _append($res, _intdiv(($n * ($n + 1)), 2));
  $n = $n + 1;
};
  return $res;
}
function parse_words($text) {
  $words = [];
  $current = '';
  $i = 0;
  while ($i < strlen($text)) {
  $c = substr($text, $i, $i + 1 - $i);
  if ($c == ',') {
  $words = _append($words, $current);
  $current = '';
} else {
  if ($c == '"') {
} else {
  if ($c == '' || $c == '
') {
} else {
  $current = $current . $c;
};
};
}
  $i = $i + 1;
};
  if (strlen($current) > 0) {
  $words = _append($words, $current);
}
  return $words;
}
function word_value($word) {
  $total = 0;
  $i = 0;
  while ($i < strlen($word)) {
  $total = $total + ord(substr($word, $i, $i + 1 - $i)) - 64;
  $i = $i + 1;
};
  return $total;
}
function mochi_contains($xs, $target) {
  foreach ($xs as $x) {
  if ($x == $target) {
  return true;
}
};
  return false;
}
function solution() {
  $text = _read_file('words.txt');
  $words = parse_words($text);
  $tri = triangular_numbers(100);
  $count = 0;
  foreach ($words as $w) {
  $v = word_value($w);
  if (mochi_contains($tri, $v)) {
  $count = $count + 1;
}
};
  return $count;
}
echo rtrim(_str(solution())), PHP_EOL;
