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
echo rtrim('Police  Sanitation  Fire'), PHP_EOL;
echo rtrim('------  ----------  ----'), PHP_EOL;
$count = 0;
$i = 2;
while ($i < 7) {
  $j = 1;
  while ($j < 8) {
  if ($j != $i) {
  $k = 1;
  while ($k < 8) {
  if ($k != $i && $k != $j) {
  if ($i + $j + $k == 12) {
  echo rtrim('  ' . _str($i) . '         ' . _str($j) . '         ' . _str($k)), PHP_EOL;
  $count = $count + 1;
};
}
  $k = $k + 1;
};
}
  $j = $j + 1;
};
  $i = $i + 2;
}
echo rtrim(''), PHP_EOL;
echo rtrim(_str($count) . ' valid combinations'), PHP_EOL;
