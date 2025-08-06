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
function allConstruct($target, $wordBank) {
  $tableSize = strlen($target) + 1;
  $table = [];
  $idx = 0;
  while ($idx < $tableSize) {
  $empty = [];
  $table = _append($table, $empty);
  $idx = $idx + 1;
};
  $base = [];
  $table[0] = [$base];
  $i = 0;
  while ($i < $tableSize) {
  if (count($table[$i]) != 0) {
  $w = 0;
  while ($w < count($wordBank)) {
  $word = $wordBank[$w];
  $wordLen = strlen($word);
  if (substr($target, $i, $i + $wordLen - $i) == $word) {
  $k = 0;
  while ($k < count($table[$i])) {
  $way = $table[$i][$k];
  $combination = [];
  $m = 0;
  while ($m < count($way)) {
  $combination = _append($combination, $way[$m]);
  $m = $m + 1;
};
  $combination = _append($combination, $word);
  $nextIndex = $i + $wordLen;
  $table[$nextIndex] = _append($table[$nextIndex], $combination);
  $k = $k + 1;
};
}
  $w = $w + 1;
};
}
  $i = $i + 1;
};
  return $table[strlen($target)];
}
echo rtrim(_str(allConstruct('jwajalapa', ['jwa', 'j', 'w', 'a', 'la', 'lapa']))), PHP_EOL;
echo rtrim(_str(allConstruct('rajamati', ['s', 'raj', 'amat', 'raja', 'ma', 'i', 't']))), PHP_EOL;
echo rtrim(_str(allConstruct('hexagonosaurus', ['h', 'ex', 'hex', 'ag', 'ago', 'ru', 'auru', 'rus', 'go', 'no', 'o', 's']))), PHP_EOL;
