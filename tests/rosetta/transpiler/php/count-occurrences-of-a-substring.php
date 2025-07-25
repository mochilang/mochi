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
function countOccurrences($s, $sub) {
  if (strlen($sub) == 0) {
  return strlen($s) + 1;
}
  $cnt = 0;
  $i = 0;
  $step = strlen($sub);
  while ($i + $step <= strlen($s)) {
  if (substr($s, $i, $i + $step - $i) == $sub) {
  $cnt = $cnt + 1;
  $i = $i + $step;
} else {
  $i = $i + 1;
}
};
  return $cnt;
}
function main() {
  echo rtrim(json_encode(_str(countOccurrences('the three truths', 'th')), 1344)), PHP_EOL;
  echo rtrim(json_encode(_str(countOccurrences('ababababab', 'abab')), 1344)), PHP_EOL;
}
main();
