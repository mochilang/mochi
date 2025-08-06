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
function distance_sq($a, $b) {
  $sum = 0.0;
  $i = 0;
  while ($i < count($a)) {
  $diff = $a[$i] - $b[$i];
  $sum = $sum + $diff * $diff;
  $i = $i + 1;
};
  return $sum;
}
function mean($vectors) {
  $dim = count($vectors[0]);
  $res = [];
  $i = 0;
  while ($i < $dim) {
  $total = 0.0;
  $j = 0;
  while ($j < count($vectors)) {
  $total = $total + $vectors[$j][$i];
  $j = $j + 1;
};
  $res = _append($res, $total / count($vectors));
  $i = $i + 1;
};
  return $res;
}
function k_means($vectors, $k, $iterations) {
  $centroids = [];
  $i = 0;
  while ($i < $k) {
  $centroids = _append($centroids, $vectors[$i]);
  $i = $i + 1;
};
  $assignments = [];
  $n = count($vectors);
  $i = 0;
  while ($i < $n) {
  $assignments = _append($assignments, 0);
  $i = $i + 1;
};
  $it = 0;
  while ($it < $iterations) {
  $v = 0;
  while ($v < $n) {
  $best = 0;
  $bestDist = distance_sq($vectors[$v], $centroids[0]);
  $c = 1;
  while ($c < $k) {
  $d = distance_sq($vectors[$v], $centroids[$c]);
  if ($d < $bestDist) {
  $bestDist = $d;
  $best = $c;
}
  $c = $c + 1;
};
  $assignments[$v] = $best;
  $v = $v + 1;
};
  $cIdx = 0;
  while ($cIdx < $k) {
  $cluster = [];
  $v2 = 0;
  while ($v2 < $n) {
  if ($assignments[$v2] == $cIdx) {
  $cluster = _append($cluster, $vectors[$v2]);
}
  $v2 = $v2 + 1;
};
  if (count($cluster) > 0) {
  $centroids[$cIdx] = mean($cluster);
}
  $cIdx = $cIdx + 1;
};
  $it = $it + 1;
};
  return ['centroids' => $centroids, 'assignments' => $assignments];
}
function main() {
  $vectors = [[1.0, 2.0], [1.5, 1.8], [5.0, 8.0], [8.0, 8.0], [1.0, 0.6], [9.0, 11.0]];
  $result = k_means($vectors, 2, 5);
  echo rtrim(_str($result['centroids'])), PHP_EOL;
  echo rtrim(_str($result['assignments'])), PHP_EOL;
}
main();
