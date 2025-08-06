<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function cross($o, $a, $b) {
  return ($a['x'] - $o['x']) * ($b['y'] - $o['y']) - ($a['y'] - $o['y']) * ($b['x'] - $o['x']);
}
function sortPoints($ps) {
  $arr = $ps;
  $n = count($arr);
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j < $n - 1) {
  $p = $arr[$j];
  $q = $arr[$j + 1];
  if ($p['x'] > $q['x'] || ($p['x'] == $q['x'] && $p['y'] > $q['y'])) {
  $arr[$j] = $q;
  $arr[$j + 1] = $p;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $arr;
}
function convex_hull($ps) {
  $ps = sortPoints($ps);
  $lower = [];
  foreach ($ps as $p) {
  while (count($lower) >= 2 && cross($lower[count($lower) - 2], $lower[count($lower) - 1], $p) <= 0) {
  $lower = array_slice($lower, 0, count($lower) - 1 - 0);
};
  $lower = _append($lower, $p);
};
  $upper = [];
  $i = count($ps) - 1;
  while ($i >= 0) {
  $p = $ps[$i];
  while (count($upper) >= 2 && cross($upper[count($upper) - 2], $upper[count($upper) - 1], $p) <= 0) {
  $upper = array_slice($upper, 0, count($upper) - 1 - 0);
};
  $upper = _append($upper, $p);
  $i = $i - 1;
};
  $hull = array_slice($lower, 0, count($lower) - 1 - 0);
  $j = 0;
  while ($j < count($upper) - 1) {
  $hull = _append($hull, $upper[$j]);
  $j = $j + 1;
};
  return $hull;
}
