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
function lower($ch) {
  global $partList, $nAssemblies, $a;
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $i = 0;
  while ($i < strlen($upper)) {
  if ($ch == substr($upper, $i, $i + 1 - $i)) {
  return substr($lower, $i, $i + 1 - $i);
}
  $i = $i + 1;
};
  return $ch;
}
$partList = ['A', 'B', 'C', 'D'];
$nAssemblies = 3;
for ($cycle = 1; $cycle < ($nAssemblies + 1); $cycle++) {
  echo rtrim('begin assembly cycle ' . _str($cycle)), PHP_EOL;
  $a = '';
  foreach ($partList as $p) {
  echo rtrim($p . ' worker begins part'), PHP_EOL;
  echo rtrim($p . ' worker completed ' . strtolower($p)), PHP_EOL;
  $a = $a . strtolower($p);
};
  echo rtrim($a . ' assembled.  cycle ' . _str($cycle) . ' complete'), PHP_EOL;
}
