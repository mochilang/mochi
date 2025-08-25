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
function two_sum($nums, $target) {
  $chk_map = [];
  $idx = 0;
  while ($idx < count($nums)) {
  $val = $nums[$idx];
  $compl = $target - $val;
  if (array_key_exists($compl, $chk_map)) {
  return [$chk_map[$compl] - 1, $idx];
}
  $chk_map[$val] = $idx + 1;
  $idx = $idx + 1;
};
  return [];
}
echo rtrim(_str(two_sum([2, 7, 11, 15], 9))), PHP_EOL;
