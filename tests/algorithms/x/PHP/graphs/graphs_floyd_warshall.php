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
$INF = 1000000000.0;
function floyd_warshall($graph) {
  global $INF, $result;
  $v = count($graph);
  $dist = [];
  $i = 0;
  while ($i < $v) {
  $row = [];
  $j = 0;
  while ($j < $v) {
  $row = _append($row, $graph[$i][$j]);
  $j = $j + 1;
};
  $dist = _append($dist, $row);
  $i = $i + 1;
};
  $k = 0;
  while ($k < $v) {
  $i = 0;
  while ($i < $v) {
  $j = 0;
  while ($j < $v) {
  if ($dist[$i][$k] < $INF && $dist[$k][$j] < $INF && $dist[$i][$k] + $dist[$k][$j] < $dist[$i][$j]) {
  $dist[$i][$j] = $dist[$i][$k] + $dist[$k][$j];
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  $k = $k + 1;
};
  return $dist;
}
function print_dist($dist) {
  global $INF, $graph, $result;
  echo rtrim('
The shortest path matrix using Floyd Warshall algorithm
'), PHP_EOL;
  $i = 0;
  while ($i < count($dist)) {
  $j = 0;
  $line = '';
  while ($j < count($dist[$i])) {
  if ($dist[$i][$j] >= $INF / 2.0) {
  $line = $line . 'INF	';
} else {
  $line = $line . _str(intval($dist[$i][$j])) . '	';
}
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
};
}
$graph = [[0.0, 5.0, $INF, 10.0], [$INF, 0.0, 3.0, $INF], [$INF, $INF, 0.0, 1.0], [$INF, $INF, $INF, 0.0]];
$result = floyd_warshall($graph);
print_dist($result);
