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
$WIDTH = 10;
$HEIGHT = 10;
$PREY_INITIAL_COUNT = 20;
$PREY_REPRODUCTION_TIME = 5;
$PREDATOR_INITIAL_COUNT = 5;
$PREDATOR_REPRODUCTION_TIME = 20;
$PREDATOR_INITIAL_ENERGY = 15;
$PREDATOR_FOOD_VALUE = 5;
$TYPE_PREY = 0;
$TYPE_PREDATOR = 1;
$seed = 123456789;
function mochi_rand() {
  global $WIDTH, $HEIGHT, $PREY_INITIAL_COUNT, $PREY_REPRODUCTION_TIME, $PREDATOR_INITIAL_COUNT, $PREDATOR_REPRODUCTION_TIME, $PREDATOR_INITIAL_ENERGY, $PREDATOR_FOOD_VALUE, $TYPE_PREY, $TYPE_PREDATOR, $seed, $board, $entities, $dr, $dc, $t;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return $seed;
}
function rand_range($max) {
  global $WIDTH, $HEIGHT, $PREY_INITIAL_COUNT, $PREY_REPRODUCTION_TIME, $PREDATOR_INITIAL_COUNT, $PREDATOR_REPRODUCTION_TIME, $PREDATOR_INITIAL_ENERGY, $PREDATOR_FOOD_VALUE, $TYPE_PREY, $TYPE_PREDATOR, $seed, $board, $entities, $dr, $dc, $t;
  return fmod(mochi_rand(), $max);
}
function mochi_shuffle(&$list_int) {
  global $WIDTH, $HEIGHT, $PREY_INITIAL_COUNT, $PREY_REPRODUCTION_TIME, $PREDATOR_INITIAL_COUNT, $PREDATOR_REPRODUCTION_TIME, $PREDATOR_INITIAL_ENERGY, $PREDATOR_FOOD_VALUE, $TYPE_PREY, $TYPE_PREDATOR, $seed, $board, $entities, $dr, $dc, $t;
  $i = count($list_int) - 1;
  while ($i > 0) {
  $j = rand_range($i + 1);
  $tmp = $list_int[$i];
  $list_int[$i] = $list_int[$j];
  $list_int[$j] = $tmp;
  $i = $i - 1;
};
  return $list_int;
}
function create_board() {
  global $WIDTH, $HEIGHT, $PREY_INITIAL_COUNT, $PREY_REPRODUCTION_TIME, $PREDATOR_INITIAL_COUNT, $PREDATOR_REPRODUCTION_TIME, $PREDATOR_INITIAL_ENERGY, $PREDATOR_FOOD_VALUE, $TYPE_PREY, $TYPE_PREDATOR, $seed, $entities, $dr, $dc, $t;
  $board = [];
  $r = 0;
  while ($r < $HEIGHT) {
  $row = [];
  $c = 0;
  while ($c < $WIDTH) {
  $row = _append($row, 0);
  $c = $c + 1;
};
  $board = _append($board, $row);
  $r = $r + 1;
};
  return $board;
}
function create_prey($r, $c) {
  global $WIDTH, $HEIGHT, $PREY_INITIAL_COUNT, $PREY_REPRODUCTION_TIME, $PREDATOR_INITIAL_COUNT, $PREDATOR_REPRODUCTION_TIME, $PREDATOR_INITIAL_ENERGY, $PREDATOR_FOOD_VALUE, $TYPE_PREY, $TYPE_PREDATOR, $seed, $board, $entities, $dr, $dc, $t;
  return [$TYPE_PREY, $r, $c, $PREY_REPRODUCTION_TIME, 0, 1];
}
function create_predator($r, $c) {
  global $WIDTH, $HEIGHT, $PREY_INITIAL_COUNT, $PREY_REPRODUCTION_TIME, $PREDATOR_INITIAL_COUNT, $PREDATOR_REPRODUCTION_TIME, $PREDATOR_INITIAL_ENERGY, $PREDATOR_FOOD_VALUE, $TYPE_PREY, $TYPE_PREDATOR, $seed, $board, $entities, $dr, $dc, $t;
  return [$TYPE_PREDATOR, $r, $c, $PREDATOR_REPRODUCTION_TIME, $PREDATOR_INITIAL_ENERGY, 1];
}
$board = create_board();
$entities = [];
function empty_cell($r, $c) {
  global $WIDTH, $HEIGHT, $PREY_INITIAL_COUNT, $PREY_REPRODUCTION_TIME, $PREDATOR_INITIAL_COUNT, $PREDATOR_REPRODUCTION_TIME, $PREDATOR_INITIAL_ENERGY, $PREDATOR_FOOD_VALUE, $TYPE_PREY, $TYPE_PREDATOR, $seed, $board, $entities, $dr, $dc, $t;
  return $board[$r][$c] == 0;
}
function add_entity($typ) {
  global $WIDTH, $HEIGHT, $PREY_INITIAL_COUNT, $PREY_REPRODUCTION_TIME, $PREDATOR_INITIAL_COUNT, $PREDATOR_REPRODUCTION_TIME, $PREDATOR_INITIAL_ENERGY, $PREDATOR_FOOD_VALUE, $TYPE_PREY, $TYPE_PREDATOR, $seed, $board, $entities, $dr, $dc, $t;
  while (true) {
  $r = rand_range($HEIGHT);
  $c = rand_range($WIDTH);
  if (empty_cell($r, $c)) {
  if ($typ == $TYPE_PREY) {
  $board[$r][$c] = 1;
  $entities = _append($entities, create_prey($r, $c));
} else {
  $board[$r][$c] = 2;
  $entities = _append($entities, create_predator($r, $c));
};
  return;
}
};
}
function setup() {
  global $WIDTH, $HEIGHT, $PREY_INITIAL_COUNT, $PREY_REPRODUCTION_TIME, $PREDATOR_INITIAL_COUNT, $PREDATOR_REPRODUCTION_TIME, $PREDATOR_INITIAL_ENERGY, $PREDATOR_FOOD_VALUE, $TYPE_PREY, $TYPE_PREDATOR, $seed, $board, $entities, $dr, $dc, $t;
  $i = 0;
  while ($i < $PREY_INITIAL_COUNT) {
  add_entity($TYPE_PREY);
  $i = $i + 1;
};
  $i = 0;
  while ($i < $PREDATOR_INITIAL_COUNT) {
  add_entity($TYPE_PREDATOR);
  $i = $i + 1;
};
}
$dr = [-1, 0, 1, 0];
$dc = [0, 1, 0, -1];
function inside($r, $c) {
  global $WIDTH, $HEIGHT, $PREY_INITIAL_COUNT, $PREY_REPRODUCTION_TIME, $PREDATOR_INITIAL_COUNT, $PREDATOR_REPRODUCTION_TIME, $PREDATOR_INITIAL_ENERGY, $PREDATOR_FOOD_VALUE, $TYPE_PREY, $TYPE_PREDATOR, $seed, $board, $entities, $dr, $dc, $t;
  return $r >= 0 && $r < $HEIGHT && $c >= 0 && $c < $WIDTH;
}
function find_prey($r, $c) {
  global $WIDTH, $HEIGHT, $PREY_INITIAL_COUNT, $PREY_REPRODUCTION_TIME, $PREDATOR_INITIAL_COUNT, $PREDATOR_REPRODUCTION_TIME, $PREDATOR_INITIAL_ENERGY, $PREDATOR_FOOD_VALUE, $TYPE_PREY, $TYPE_PREDATOR, $seed, $board, $entities, $dr, $dc, $t;
  $i = 0;
  while ($i < count($entities)) {
  $e = $entities[$i];
  if ($e[5] == 1 && $e[0] == $TYPE_PREY && $e[1] == $r && $e[2] == $c) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function step_world() {
  global $WIDTH, $HEIGHT, $PREY_INITIAL_COUNT, $PREY_REPRODUCTION_TIME, $PREDATOR_INITIAL_COUNT, $PREDATOR_REPRODUCTION_TIME, $PREDATOR_INITIAL_ENERGY, $PREDATOR_FOOD_VALUE, $TYPE_PREY, $TYPE_PREDATOR, $seed, $board, $entities, $dr, $dc, $t;
  $i = 0;
  while ($i < count($entities)) {
  $e = $entities[$i];
  if ($e[5] == 0) {
  $i = $i + 1;
  continue;
}
  $typ = $e[0];
  $row = $e[1];
  $col = $e[2];
  $repro = $e[3];
  $energy = $e[4];
  $dirs = [0, 1, 2, 3];
  $dirs = mochi_shuffle($dirs);
  $moved = false;
  $old_r = $row;
  $old_c = $col;
  if ($typ == $TYPE_PREDATOR) {
  $j = 0;
  $ate = false;
  while ($j < 4) {
  $d = $dirs[$j];
  $nr = $row + $dr[$d];
  $nc = $col + $dc[$d];
  if (inside($nr, $nc) && $board[$nr][$nc] == 1) {
  $prey_index = find_prey($nr, $nc);
  if ($prey_index >= 0) {
  $entities[$prey_index][5] = 0;
};
  $board[$nr][$nc] = 2;
  $board[$row][$col] = 0;
  $e[1] = $nr;
  $e[2] = $nc;
  $e[4] = $energy + $PREDATOR_FOOD_VALUE - 1;
  $moved = true;
  $ate = true;
  break;
}
  $j = $j + 1;
};
  if (!$ate) {
  $j = 0;
  while ($j < 4) {
  $d = $dirs[$j];
  $nr = $row + $dr[$d];
  $nc = $col + $dc[$d];
  if (inside($nr, $nc) && $board[$nr][$nc] == 0) {
  $board[$nr][$nc] = 2;
  $board[$row][$col] = 0;
  $e[1] = $nr;
  $e[2] = $nc;
  $moved = true;
  break;
}
  $j = $j + 1;
};
  $e[4] = $energy - 1;
};
  if ($e[4] <= 0) {
  $e[5] = 0;
  $board[$e[1]][$e[2]] = 0;
};
} else {
  $j = 0;
  while ($j < 4) {
  $d = $dirs[$j];
  $nr = $row + $dr[$d];
  $nc = $col + $dc[$d];
  if (inside($nr, $nc) && $board[$nr][$nc] == 0) {
  $board[$nr][$nc] = 1;
  $board[$row][$col] = 0;
  $e[1] = $nr;
  $e[2] = $nc;
  $moved = true;
  break;
}
  $j = $j + 1;
};
}
  if ($e[5] == 1) {
  if ($moved && $repro <= 0) {
  if ($typ == $TYPE_PREY) {
  $board[$old_r][$old_c] = 1;
  $entities = _append($entities, create_prey($old_r, $old_c));
  $e[3] = $PREY_REPRODUCTION_TIME;
} else {
  $board[$old_r][$old_c] = 2;
  $entities = _append($entities, create_predator($old_r, $old_c));
  $e[3] = $PREDATOR_REPRODUCTION_TIME;
};
} else {
  $e[3] = $repro - 1;
};
}
  $i = $i + 1;
};
  $alive = [];
  $k = 0;
  while ($k < count($entities)) {
  $e2 = $entities[$k];
  if ($e2[5] == 1) {
  $alive = _append($alive, $e2);
}
  $k = $k + 1;
};
  $entities = $alive;
}
function count_entities($typ) {
  global $WIDTH, $HEIGHT, $PREY_INITIAL_COUNT, $PREY_REPRODUCTION_TIME, $PREDATOR_INITIAL_COUNT, $PREDATOR_REPRODUCTION_TIME, $PREDATOR_INITIAL_ENERGY, $PREDATOR_FOOD_VALUE, $TYPE_PREY, $TYPE_PREDATOR, $seed, $board, $entities, $dr, $dc, $t;
  $cnt = 0;
  $i = 0;
  while ($i < count($entities)) {
  if ($entities[$i][0] == $typ && $entities[$i][5] == 1) {
  $cnt = $cnt + 1;
}
  $i = $i + 1;
};
  return $cnt;
}
setup();
$t = 0;
while ($t < 10) {
  step_world();
  $t = $t + 1;
}
echo rtrim('Prey: ' . _str(count_entities($TYPE_PREY))), PHP_EOL;
echo rtrim('Predators: ' . _str(count_entities($TYPE_PREDATOR))), PHP_EOL;
