<?php
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
function sqrtApprox($x) {
  global $cities;
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
}
function rand_float() {
  global $cities;
  return floatval((fmod(_now(), 1000000))) / 1000000.0;
}
function pow_float($base, $exp) {
  global $cities;
  $result = 1.0;
  $i = 0;
  $e = intval($exp);
  while ($i < $e) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
}
function distance($city1, $city2) {
  global $cities;
  $dx = floatval(($city1[0] - $city2[0]));
  $dy = floatval(($city1[1] - $city2[1]));
  return sqrtApprox($dx * $dx + $dy * $dy);
}
function choose_weighted($options, $weights) {
  global $cities;
  $total = 0.0;
  $i = 0;
  while ($i < count($weights)) {
  $total = $total + $weights[$i];
  $i = $i + 1;
};
  $r = rand_float() * $total;
  $accum = 0.0;
  $i = 0;
  while ($i < count($weights)) {
  $accum = $accum + $weights[$i];
  if ($r <= $accum) {
  return $options[$i];
}
  $i = $i + 1;
};
  return $options[count($options) - 1];
}
function city_select($pheromone, $current, $unvisited, $alpha, $beta, $cities) {
  $probs = [];
  $i = 0;
  while ($i < count($unvisited)) {
  $city = $unvisited[$i];
  $dist = distance($cities[$city], $cities[$current]);
  $trail = $pheromone[$city][$current];
  $prob = pow_float($trail, $alpha) * pow_float(1.0 / $dist, $beta);
  $probs = _append($probs, $prob);
  $i = $i + 1;
};
  return choose_weighted($unvisited, $probs);
}
function pheromone_update(&$pheromone, $cities, $evaporation, $ants_route, $q) {
  $n = count($pheromone);
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j < $n) {
  $pheromone[$i][$j] = $pheromone[$i][$j] * $evaporation;
  $j = $j + 1;
};
  $i = $i + 1;
};
  $a = 0;
  while ($a < count($ants_route)) {
  $route = $ants_route[$a];
  $total = 0.0;
  $r = 0;
  while ($r < count($route) - 1) {
  $total = $total + distance($cities[$route[$r]], $cities[$route[$r + 1]]);
  $r = $r + 1;
};
  $delta = $q / $total;
  $r = 0;
  while ($r < count($route) - 1) {
  $u = $route[$r];
  $v = $route[$r + 1];
  $pheromone[$u][$v] = $pheromone[$u][$v] + $delta;
  $pheromone[$v][$u] = $pheromone[$u][$v];
  $r = $r + 1;
};
  $a = $a + 1;
};
  return $pheromone;
}
function remove_value($lst, $val) {
  global $cities;
  $res = [];
  $i = 0;
  while ($i < count($lst)) {
  if ($lst[$i] != $val) {
  $res = _append($res, $lst[$i]);
}
  $i = $i + 1;
};
  return $res;
}
function ant_colony($cities, $ants_num, $iterations, $evaporation, $alpha, $beta, $q) {
  $n = count($cities);
  $pheromone = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $row = _append($row, 1.0);
  $j = $j + 1;
};
  $pheromone = _append($pheromone, $row);
  $i = $i + 1;
};
  $best_path = [];
  $best_distance = 1000000000.0;
  $iter = 0;
  while ($iter < $iterations) {
  $ants_route = [];
  $k = 0;
  while ($k < $ants_num) {
  $route = [0];
  $unvisited = [];
  foreach (array_keys($cities) as $key) {
  if ($key != 0) {
  $unvisited = _append($unvisited, $key);
}
};
  $current = 0;
  while (count($unvisited) > 0) {
  $next_city = city_select($pheromone, $current, $unvisited, $alpha, $beta, $cities);
  $route = _append($route, $next_city);
  $unvisited = remove_value($unvisited, $next_city);
  $current = $next_city;
};
  $route = _append($route, 0);
  $ants_route = _append($ants_route, $route);
  $k = $k + 1;
};
  $pheromone = pheromone_update($pheromone, $cities, $evaporation, $ants_route, $q);
  $a = 0;
  while ($a < count($ants_route)) {
  $route = $ants_route[$a];
  $dist = 0.0;
  $r = 0;
  while ($r < count($route) - 1) {
  $dist = $dist + distance($cities[$route[$r]], $cities[$route[$r + 1]]);
  $r = $r + 1;
};
  if ($dist < $best_distance) {
  $best_distance = $dist;
  $best_path = $route;
}
  $a = $a + 1;
};
  $iter = $iter + 1;
};
  echo rtrim('best_path = ' . _str($best_path)), PHP_EOL;
  echo rtrim('best_distance = ' . _str($best_distance)), PHP_EOL;
}
$cities = [0 => [0, 0], 1 => [0, 5], 2 => [3, 8], 3 => [8, 10], 4 => [12, 8], 5 => [12, 4], 6 => [8, 0], 7 => [6, 2]];
ant_colony($cities, 10, 20, 0.7, 1.0, 5.0, 10.0);
