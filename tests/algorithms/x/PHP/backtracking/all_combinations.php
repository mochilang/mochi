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
function create_all_state($increment, $total, $level, $current, $result) {
  if ($level == 0) {
  return _append($result, $current);
}
  $i = $increment;
  while ($i <= $total - $level + 1) {
  $next_current = _append($current, $i);
  $result = create_all_state($i + 1, $total, $level - 1, $next_current, $result);
  $i = $i + 1;
};
  return $result;
}
function generate_all_combinations($n, $k) {
  if ($k < 0 || $n < 0) {
  return [];
}
  $result = [];
  return create_all_state(1, $n, $k, [], $result);
}
echo rtrim(_str(generate_all_combinations(4, 2))), PHP_EOL;
echo rtrim(_str(generate_all_combinations(3, 1))), PHP_EOL;
