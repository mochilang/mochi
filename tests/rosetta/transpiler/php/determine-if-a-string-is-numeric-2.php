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
function isInt($s) {
  if (strlen($s) == 0) {
  return false;
}
  foreach (str_split($s) as $ch) {
  if ($ch < '0' || $ch > '9') {
  return false;
}
};
  return true;
}
function main() {
  echo rtrim('Are these strings integers?'), PHP_EOL;
  $v = '1';
  $b = false;
  if (isInt($v)) {
  $b = true;
}
  echo rtrim('  ' . $v . ' -> ' . _str($b)), PHP_EOL;
  $i = 'one';
  echo rtrim('  ' . $i . ' -> ' . _str(isInt($i))), PHP_EOL;
}
main();
