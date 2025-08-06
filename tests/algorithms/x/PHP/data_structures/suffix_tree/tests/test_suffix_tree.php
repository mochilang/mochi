<?php
ini_set('memory_limit', '-1');
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
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
function suffix_tree_new($text) {
  global $st, $patterns_exist, $i, $patterns_none, $substrings;
  return ['text' => $text];
}
function suffix_tree_search($st, $pattern) {
  global $text, $patterns_exist, $patterns_none, $substrings;
  if (strlen($pattern) == 0) {
  return true;
}
  $i = 0;
  $n = _len($st['text']);
  $m = strlen($pattern);
  while ($i <= $n - $m) {
  $j = 0;
  $found = true;
  while ($j < $m) {
  if ($st['text'][$i + $j] != substr($pattern, $j, $j + 1 - $j)) {
  $found = false;
  break;
}
  $j = $j + 1;
};
  if ($found) {
  return true;
}
  $i = $i + 1;
};
  return false;
}
$text = 'banana';
$st = suffix_tree_new($text);
$patterns_exist = ['ana', 'ban', 'na'];
$i = 0;
while ($i < count($patterns_exist)) {
  echo rtrim(_str(suffix_tree_search($st, $patterns_exist[$i]))), PHP_EOL;
  $i = $i + 1;
}
$patterns_none = ['xyz', 'apple', 'cat'];
$i = 0;
while ($i < count($patterns_none)) {
  echo rtrim(_str(suffix_tree_search($st, $patterns_none[$i]))), PHP_EOL;
  $i = $i + 1;
}
echo rtrim(_str(suffix_tree_search($st, ''))), PHP_EOL;
echo rtrim(_str(suffix_tree_search($st, $text))), PHP_EOL;
$substrings = ['ban', 'ana', 'a', 'na'];
$i = 0;
while ($i < count($substrings)) {
  echo rtrim(_str(suffix_tree_search($st, $substrings[$i]))), PHP_EOL;
  $i = $i + 1;
}
