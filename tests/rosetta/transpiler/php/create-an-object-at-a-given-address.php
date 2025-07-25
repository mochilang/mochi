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
function listStr($xs) {
  $s = '[';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . _str($xs[$i]);
  if ($i < count($xs) - 1) {
  $s = $s . ' ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
}
function pointerDemo() {
  echo rtrim('Pointer:'), PHP_EOL;
  $i = 0;
  echo rtrim('Before:'), PHP_EOL;
  echo rtrim('	<address>: ' . _str($i) . ', ' . _str($i)), PHP_EOL;
  $i = 3;
  echo rtrim('After:'), PHP_EOL;
  echo rtrim('	<address>: ' . _str($i) . ', ' . _str($i)), PHP_EOL;
}
function sliceDemo() {
  echo rtrim('Slice:'), PHP_EOL;
  $a = [];
  for ($_ = 0; $_ < 10; $_++) {
  $a = array_merge($a, [0]);
};
  $s = $a;
  echo rtrim('Before:'), PHP_EOL;
  echo rtrim('	s: ' . listStr($s)), PHP_EOL;
  echo rtrim('	a: ' . listStr($a)), PHP_EOL;
  $data = [65, 32, 115, 116, 114, 105, 110, 103, 46];
  $idx = 0;
  while ($idx < count($data)) {
  $s[$idx] = $data[$idx];
  $idx = $idx + 1;
};
  echo rtrim('After:'), PHP_EOL;
  echo rtrim('	s: ' . listStr($s)), PHP_EOL;
  echo rtrim('	a: ' . listStr($a)), PHP_EOL;
}
pointerDemo();
echo rtrim(''), PHP_EOL;
sliceDemo();
