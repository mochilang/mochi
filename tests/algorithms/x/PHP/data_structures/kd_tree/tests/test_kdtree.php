<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  $INF = 1000000000.0;
  $seed = 1;
  function rand_float() {
  global $INF, $seed;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return (floatval($seed)) / 2147483648.0;
};
  function hypercube_points($num_points, $cube_size, $num_dimensions) {
  global $INF, $seed;
  $pts = [];
  $i = 0;
  while ($i < $num_points) {
  $p = [];
  $j = 0;
  while ($j < $num_dimensions) {
  $v = $cube_size * rand_float();
  $p = _append($p, $v);
  $j = $j + 1;
};
  $pts = _append($pts, $p);
  $i = $i + 1;
};
  return $pts;
};
  function build_kdtree($points, $depth) {
  global $INF, $seed;
  return $points;
};
  function distance_sq($a, $b) {
  global $INF, $seed;
  $sum = 0.0;
  $i = 0;
  while ($i < count($a)) {
  $d = $a[$i] - $b[$i];
  $sum = $sum + $d * $d;
  $i = $i + 1;
};
  return $sum;
};
  function nearest_neighbour_search($points, $query) {
  global $INF, $seed;
  if (count($points) == 0) {
  return ['dist' => $INF, 'index' => -1.0, 'visited' => 0.0];
}
  $nearest_idx = 0;
  $nearest_dist = $INF;
  $visited = 0;
  $i = 0;
  while ($i < count($points)) {
  $d = distance_sq($query, $points[$i]);
  $visited = $visited + 1;
  if ($d < $nearest_dist) {
  $nearest_dist = $d;
  $nearest_idx = $i;
}
  $i = $i + 1;
};
  return ['dist' => $nearest_dist, 'index' => floatval($nearest_idx), 'visited' => floatval($visited)];
};
  function test_build_cases() {
  global $INF, $seed;
  $empty_pts = [];
  $tree0 = build_kdtree($empty_pts, 0);
  if (count($tree0) == 0) {
  echo rtrim('case1 true'), PHP_EOL;
} else {
  echo rtrim('case1 false'), PHP_EOL;
}
  $pts1 = hypercube_points(10, 10.0, 2);
  $tree1 = build_kdtree($pts1, 2);
  if (count($tree1) > 0 && count($tree1[0]) == 2) {
  echo rtrim('case2 true'), PHP_EOL;
} else {
  echo rtrim('case2 false'), PHP_EOL;
}
  $pts2 = hypercube_points(10, 10.0, 3);
  $tree2 = build_kdtree($pts2, -2);
  if (count($tree2) > 0 && count($tree2[0]) == 3) {
  echo rtrim('case3 true'), PHP_EOL;
} else {
  echo rtrim('case3 false'), PHP_EOL;
}
};
  function test_search() {
  global $INF, $seed;
  $pts = hypercube_points(10, 10.0, 2);
  $tree = build_kdtree($pts, 0);
  $qp = hypercube_points(1, 10.0, 2)[0];
  $res = nearest_neighbour_search($tree, $qp);
  if ($res['index'] != (-1.0) && $res['dist'] >= 0.0 && $res['visited'] > 0.0) {
  echo rtrim('search true'), PHP_EOL;
} else {
  echo rtrim('search false'), PHP_EOL;
}
};
  function test_edge() {
  global $INF, $seed;
  $empty_pts = [];
  $tree = build_kdtree($empty_pts, 0);
  $query = [0.0, 0.0];
  $res = nearest_neighbour_search($tree, $query);
  if ($res['index'] == (-1.0) && $res['dist'] > 100000000.0 && $res['visited'] == 0.0) {
  echo rtrim('edge true'), PHP_EOL;
} else {
  echo rtrim('edge false'), PHP_EOL;
}
};
  function main() {
  global $INF, $seed;
  $seed = 1;
  test_build_cases();
  test_search();
  test_edge();
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
