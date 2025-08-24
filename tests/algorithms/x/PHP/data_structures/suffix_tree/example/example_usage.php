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
function new_suffix_tree($text) {
  return ['text' => $text];
}
function search($tree, $pattern) {
  $n = strlen($tree['text']);
  $m = strlen($pattern);
  if ($m == 0) {
  return true;
}
  if ($m > $n) {
  return false;
}
  $i = 0;
  while ($i <= $n - $m) {
  if (substr($tree['text'], $i, $i + $m - $i) == $pattern) {
  return true;
}
  $i = $i + 1;
};
  return false;
}
function main() {
  $text = 'monkey banana';
  $suffix_tree = new_suffix_tree($text);
  $patterns = ['ana', 'ban', 'na', 'xyz', 'mon'];
  $i = 0;
  while ($i < count($patterns)) {
  $pattern = $patterns[$i];
  $found = search($suffix_tree, $pattern);
  echo rtrim('Pattern \'' . $pattern . '\' found: ' . _str($found)), PHP_EOL;
  $i = $i + 1;
};
}
main();
