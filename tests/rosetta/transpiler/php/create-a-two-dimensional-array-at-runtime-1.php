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
function main() {
  echo rtrim('enter rows cols: '), PHP_EOL;
  $row = intval(trim(fgets(STDIN)));
  $col = intval(trim(fgets(STDIN)));
  $a = [];
  $i = 0;
  while ($i < $row) {
  $rowArr = [];
  $j = 0;
  while ($j < $col) {
  $rowArr = array_merge($rowArr, [0]);
  $j = $j + 1;
};
  $a = array_merge($a, [$rowArr]);
  $i = $i + 1;
};
  echo rtrim('a[0][0] = ' . _str($a[0][0])), PHP_EOL;
  $a[intval(($row - 1))][intval(($col - 1))] = 7;
  echo rtrim('a[' . _str($row - 1) . '][' . _str($col - 1) . '] = ' . _str($a[intval(($row - 1))][intval(($col - 1))])), PHP_EOL;
  $a = null;
}
main();
