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
$seed = 1;
$NEG_ONE = -1;
function mochi_rand() {
  global $seed, $NEG_ONE;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return $seed;
}
function randint($a, $b) {
  global $seed, $NEG_ONE;
  $r = mochi_rand();
  return $a + $r % ($b - $a + 1);
}
function random() {
  global $seed, $NEG_ONE;
  return (1.0 * mochi_rand()) / 2147483648.0;
}
function construct_highway($number_of_cells, $frequency, $initial_speed, $random_frequency, $random_speed, $max_speed) {
  global $seed, $NEG_ONE;
  $row = [];
  $i = 0;
  while ($i < $number_of_cells) {
  $row = _append($row, -1);
  $i = $i + 1;
};
  $highway = [];
  $highway = _append($highway, $row);
  $i = 0;
  if ($initial_speed < 0) {
  $initial_speed = 0;
}
  while ($i < $number_of_cells) {
  $speed = $initial_speed;
  if ($random_speed) {
  $speed = randint(0, $max_speed);
}
  $highway[0][$i] = $speed;
  $step = $frequency;
  if ($random_frequency) {
  $step = randint(1, $max_speed * 2);
}
  $i = $i + $step;
};
  return $highway;
}
function get_distance($highway_now, $car_index) {
  global $seed, $NEG_ONE;
  $distance = 0;
  $i = $car_index + 1;
  while ($i < count($highway_now)) {
  if ($highway_now[$i] > $NEG_ONE) {
  return $distance;
}
  $distance = $distance + 1;
  $i = $i + 1;
};
  return $distance + get_distance($highway_now, -1);
}
function update($highway_now, $probability, $max_speed) {
  global $seed, $NEG_ONE;
  $number_of_cells = count($highway_now);
  $next_highway = [];
  $i = 0;
  while ($i < $number_of_cells) {
  $next_highway = _append($next_highway, -1);
  $i = $i + 1;
};
  $car_index = 0;
  while ($car_index < $number_of_cells) {
  $speed = $highway_now[$car_index];
  if ($speed > $NEG_ONE) {
  $new_speed = $speed + 1;
  if ($new_speed > $max_speed) {
  $new_speed = $max_speed;
};
  $dn = get_distance($highway_now, $car_index) - 1;
  if ($new_speed > $dn) {
  $new_speed = $dn;
};
  if (random() < $probability) {
  $new_speed = $new_speed - 1;
  if ($new_speed < 0) {
  $new_speed = 0;
};
};
  $next_highway[$car_index] = $new_speed;
}
  $car_index = $car_index + 1;
};
  return $next_highway;
}
function simulate($highway, $number_of_update, $probability, $max_speed) {
  global $seed, $NEG_ONE;
  $number_of_cells = count($highway[0]);
  $i = 0;
  while ($i < $number_of_update) {
  $next_speeds = update($highway[$i], $probability, $max_speed);
  $real_next = [];
  $j = 0;
  while ($j < $number_of_cells) {
  $real_next = _append($real_next, -1);
  $j = $j + 1;
};
  $k = 0;
  while ($k < $number_of_cells) {
  $speed = $next_speeds[$k];
  if ($speed > $NEG_ONE) {
  $index = ($k + $speed) % $number_of_cells;
  $real_next[$index] = $speed;
}
  $k = $k + 1;
};
  $highway = _append($highway, $real_next);
  $i = $i + 1;
};
  return $highway;
}
function main() {
  global $seed, $NEG_ONE;
  $ex1 = simulate(construct_highway(6, 3, 0, false, false, 2), 2, 0.0, 2);
  echo rtrim(_str($ex1)), PHP_EOL;
  $ex2 = simulate(construct_highway(5, 2, -2, false, false, 2), 3, 0.0, 2);
  echo rtrim(_str($ex2)), PHP_EOL;
}
main();
