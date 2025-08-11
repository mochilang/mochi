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
function read_file($path) {
    $data = @file_get_contents($path);
    if ($data === false) {
        $alt = __DIR__ . '/../../../../github/TheAlgorithms/Mochi/' . basename(__DIR__) . '/' . $path;
        $data = @file_get_contents($alt);
        if ($data === false) return '';
    }
    return rtrim($data);
}
function split($s, $sep) {
  global $word_by_signature;
  $res = [];
  $current = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, _iadd($i, 1) - $i);
  if ($ch == $sep) {
  $res = _append($res, $current);
  $current = '';
} else {
  $current = $current . $ch;
}
  $i = _iadd($i, 1);
};
  $res = _append($res, $current);
  return $res;
}
function insertion_sort($arr) {
  global $word_by_signature;
  $a = $arr;
  $i = 1;
  while ($i < count($a)) {
  $key = $a[$i];
  $j = _isub($i, 1);
  while ($j >= 0 && $a[$j] > $key) {
  $a[_iadd($j, 1)] = $a[$j];
  $j = _isub($j, 1);
};
  $a[_iadd($j, 1)] = $key;
  $i = _iadd($i, 1);
};
  return $a;
}
function sort_chars($word) {
  global $word_by_signature;
  $chars = [];
  $i = 0;
  while ($i < strlen($word)) {
  $chars = _append($chars, $word[$i]);
  $i = _iadd($i, 1);
};
  $chars = insertion_sort($chars);
  $res = '';
  $i = 0;
  while ($i < count($chars)) {
  $res = $res . $chars[$i];
  $i = _iadd($i, 1);
};
  return $res;
}
function unique_sorted($words) {
  global $word_by_signature;
  $seen = [];
  $res = [];
  foreach ($words as $w) {
  if ($w != '' && !(array_key_exists($w, $seen))) {
  $res = _append($res, $w);
  $seen[$w] = true;
}
};
  $res = insertion_sort($res);
  return $res;
}
$word_by_signature = [];
function build_map($words) {
  global $word_by_signature;
  foreach ($words as $w) {
  $sig = sort_chars($w);
  $arr = [];
  if (array_key_exists($sig, $word_by_signature)) {
  $arr = $word_by_signature[$sig];
}
  $arr = _append($arr, $w);
  $word_by_signature[$sig] = $arr;
};
}
function anagram($my_word) {
  global $word_by_signature;
  $sig = sort_chars($my_word);
  if (array_key_exists($sig, $word_by_signature)) {
  return $word_by_signature[$sig];
}
  return [];
}
function main() {
  global $word_by_signature;
  $text = read_file('words.txt');
  $lines = split($text, '
');
  $words = unique_sorted($lines);
  build_map($words);
  foreach ($words as $w) {
  $anas = anagram($w);
  if (count($anas) > 1) {
  $line = $w . ':';
  $i = 0;
  while ($i < count($anas)) {
  if ($i > 0) {
  $line = $line . ',';
}
  $line = $line . $anas[$i];
  $i = _iadd($i, 1);
};
  echo rtrim($line), PHP_EOL;
}
};
}
main();
