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
function count_assignments($person, $task_performed, $used) {
  if ($person == count($task_performed)) {
  return 1;
}
  $total = 0;
  $tasks = $task_performed[$person];
  $i = 0;
  while ($i < count($tasks)) {
  $t = $tasks[$i];
  if (!(in_array($t, $used))) {
  $total = $total + count_assignments($person + 1, $task_performed, _append($used, $t));
}
  $i = $i + 1;
};
  return $total;
}
function count_no_of_ways($task_performed) {
  return count_assignments(0, $task_performed, []);
}
function main() {
  $task_performed = [[1, 3, 4], [1, 2, 5], [3, 4]];
  echo rtrim(_str(count_no_of_ways($task_performed))), PHP_EOL;
}
main();
