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
function repeat($s, $n) {
    return str_repeat($s, intval($n));
}
$__start_mem = memory_get_usage();
$__start = _now();
  function pow10($n) {
  global $house1, $house2, $h1_bedrooms, $h1_bathrooms, $h1_attic, $h1_kitchen, $h1_living_rooms, $h1_basement, $h1_garage, $h1_garden, $h2_upstairs, $h2_groundfloor, $h2_basement, $h1_bathroom1, $h1_bathroom2, $h1_outside, $h1_lounge, $h1_dining, $h1_conservatory, $h1_playroom, $h2_bedrooms, $h2_bathroom, $h2_toilet, $h2_attics, $h2_kitchen, $h2_living_rooms, $h2_wet_room, $h2_garage, $h2_garden, $h2_hot_tub, $h2_cellars, $h2_wine_cellar, $h2_cinema, $h2_suite1, $h2_suite2, $h2_bedroom3, $h2_bedroom4, $h2_lounge, $h2_dining, $h2_conservatory, $h2_playroom;
  $r = 1.0;
  $i = 0;
  while ($i < $n) {
  $r = $r * 10.0;
  $i = $i + 1;
};
  return $r;
};
  function formatFloat($f, $prec) {
  global $house1, $house2, $h1_bedrooms, $h1_bathrooms, $h1_attic, $h1_kitchen, $h1_living_rooms, $h1_basement, $h1_garage, $h1_garden, $h2_upstairs, $h2_groundfloor, $h2_basement, $h1_bathroom1, $h1_bathroom2, $h1_outside, $h1_lounge, $h1_dining, $h1_conservatory, $h1_playroom, $h2_bedrooms, $h2_bathroom, $h2_toilet, $h2_attics, $h2_kitchen, $h2_living_rooms, $h2_wet_room, $h2_garage, $h2_garden, $h2_hot_tub, $h2_cellars, $h2_wine_cellar, $h2_cinema, $h2_suite1, $h2_suite2, $h2_bedroom3, $h2_bedroom4, $h2_lounge, $h2_dining, $h2_conservatory, $h2_playroom;
  $scale = pow10($prec);
  $scaled = ($f * $scale) + 0.5;
  $n = (intval($scaled));
  $digits = _str($n);
  while (strlen($digits) <= $prec) {
  $digits = '0' . $digits;
};
  $intPart = substr($digits, 0, strlen($digits) - $prec - 0);
  $fracPart = substr($digits, strlen($digits) - $prec, strlen($digits) - (strlen($digits) - $prec));
  return $intPart . '.' . $fracPart;
};
  function padLeft($s, $w) {
  global $house1, $house2, $h1_bedrooms, $h1_bathrooms, $h1_attic, $h1_kitchen, $h1_living_rooms, $h1_basement, $h1_garage, $h1_garden, $h2_upstairs, $h2_groundfloor, $h2_basement, $h1_bathroom1, $h1_bathroom2, $h1_outside, $h1_lounge, $h1_dining, $h1_conservatory, $h1_playroom, $h2_bedrooms, $h2_bathroom, $h2_toilet, $h2_attics, $h2_kitchen, $h2_living_rooms, $h2_wet_room, $h2_garage, $h2_garden, $h2_hot_tub, $h2_cellars, $h2_wine_cellar, $h2_cinema, $h2_suite1, $h2_suite2, $h2_bedroom3, $h2_bedroom4, $h2_lounge, $h2_dining, $h2_conservatory, $h2_playroom;
  $res = '';
  $n = $w - strlen($s);
  while ($n > 0) {
  $res = $res . ' ';
  $n = $n - 1;
};
  return $res . $s;
};
  function mochi_repeat($ch, $n) {
  global $house1, $house2, $h1_bedrooms, $h1_bathrooms, $h1_attic, $h1_kitchen, $h1_living_rooms, $h1_basement, $h1_garage, $h1_garden, $h2_upstairs, $h2_groundfloor, $h2_basement, $h1_bathroom1, $h1_bathroom2, $h1_outside, $h1_lounge, $h1_dining, $h1_conservatory, $h1_playroom, $h2_bedrooms, $h2_bathroom, $h2_toilet, $h2_attics, $h2_kitchen, $h2_living_rooms, $h2_wet_room, $h2_garage, $h2_garden, $h2_hot_tub, $h2_cellars, $h2_wine_cellar, $h2_cinema, $h2_suite1, $h2_suite2, $h2_bedroom3, $h2_bedroom4, $h2_lounge, $h2_dining, $h2_conservatory, $h2_playroom;
  $s = '';
  $i = 0;
  while ($i < $n) {
  $s = $s . $ch;
  $i = $i + 1;
};
  return $s;
};
  function toFloat($i) {
  global $house1, $house2, $h1_bedrooms, $h1_bathrooms, $h1_attic, $h1_kitchen, $h1_living_rooms, $h1_basement, $h1_garage, $h1_garden, $h2_upstairs, $h2_groundfloor, $h2_basement, $h1_bathroom1, $h1_bathroom2, $h1_outside, $h1_lounge, $h1_dining, $h1_conservatory, $h1_playroom, $h2_bedrooms, $h2_bathroom, $h2_toilet, $h2_attics, $h2_kitchen, $h2_living_rooms, $h2_wet_room, $h2_garage, $h2_garden, $h2_hot_tub, $h2_cellars, $h2_wine_cellar, $h2_cinema, $h2_suite1, $h2_suite2, $h2_bedroom3, $h2_bedroom4, $h2_lounge, $h2_dining, $h2_conservatory, $h2_playroom;
  return floatval($i);
};
  function newNode($name, $weight, $coverage) {
  global $house1, $house2, $h1_bedrooms, $h1_bathrooms, $h1_attic, $h1_kitchen, $h1_living_rooms, $h1_basement, $h1_garage, $h1_garden, $h2_upstairs, $h2_groundfloor, $h2_basement, $h1_bathroom1, $h1_bathroom2, $h1_outside, $h1_lounge, $h1_dining, $h1_conservatory, $h1_playroom, $h2_bedrooms, $h2_bathroom, $h2_toilet, $h2_attics, $h2_kitchen, $h2_living_rooms, $h2_wet_room, $h2_garage, $h2_garden, $h2_hot_tub, $h2_cellars, $h2_wine_cellar, $h2_cinema, $h2_suite1, $h2_suite2, $h2_bedroom3, $h2_bedroom4, $h2_lounge, $h2_dining, $h2_conservatory, $h2_playroom;
  return ['name' => $name, 'weight' => $weight, 'coverage' => $coverage, 'children' => []];
};
  function addChildren(&$n, $nodes) {
  global $house1, $house2, $h1_bedrooms, $h1_bathrooms, $h1_attic, $h1_kitchen, $h1_living_rooms, $h1_basement, $h1_garage, $h1_garden, $h2_upstairs, $h2_groundfloor, $h2_basement, $h1_bathroom1, $h1_bathroom2, $h1_outside, $h1_lounge, $h1_dining, $h1_conservatory, $h1_playroom, $h2_bedrooms, $h2_bathroom, $h2_toilet, $h2_attics, $h2_kitchen, $h2_living_rooms, $h2_wet_room, $h2_garage, $h2_garden, $h2_hot_tub, $h2_cellars, $h2_wine_cellar, $h2_cinema, $h2_suite1, $h2_suite2, $h2_bedroom3, $h2_bedroom4, $h2_lounge, $h2_dining, $h2_conservatory, $h2_playroom;
  $cs = $n['children'];
  foreach ($nodes as $node) {
  $cs = array_merge($cs, [$node]);
};
  $n['children'] = $cs;
};
  function setCoverage(&$n, $value) {
  global $house1, $house2, $h1_bedrooms, $h1_bathrooms, $h1_attic, $h1_kitchen, $h1_living_rooms, $h1_basement, $h1_garage, $h1_garden, $h2_upstairs, $h2_groundfloor, $h2_basement, $h1_bathroom1, $h1_bathroom2, $h1_outside, $h1_lounge, $h1_dining, $h1_conservatory, $h1_playroom, $h2_bedrooms, $h2_bathroom, $h2_toilet, $h2_attics, $h2_kitchen, $h2_living_rooms, $h2_wet_room, $h2_garage, $h2_garden, $h2_hot_tub, $h2_cellars, $h2_wine_cellar, $h2_cinema, $h2_suite1, $h2_suite2, $h2_bedroom3, $h2_bedroom4, $h2_lounge, $h2_dining, $h2_conservatory, $h2_playroom;
  $n['coverage'] = $value;
};
  function computeCoverage($n) {
  global $house1, $house2, $h1_bedrooms, $h1_bathrooms, $h1_attic, $h1_kitchen, $h1_living_rooms, $h1_basement, $h1_garage, $h1_garden, $h2_upstairs, $h2_groundfloor, $h2_basement, $h1_bathroom1, $h1_bathroom2, $h1_outside, $h1_lounge, $h1_dining, $h1_conservatory, $h1_playroom, $h2_bedrooms, $h2_bathroom, $h2_toilet, $h2_attics, $h2_kitchen, $h2_living_rooms, $h2_wet_room, $h2_garage, $h2_garden, $h2_hot_tub, $h2_cellars, $h2_wine_cellar, $h2_cinema, $h2_suite1, $h2_suite2, $h2_bedroom3, $h2_bedroom4, $h2_lounge, $h2_dining, $h2_conservatory, $h2_playroom;
  $cs = $n['children'];
  if (count($cs) == 0) {
  return floatval($n['coverage']);
}
  $v1 = 0.0;
  $v2 = 0;
  foreach ($cs as $node) {
  $m = $node;
  $c = computeCoverage($m);
  $v1 = $v1 + toFloat(intval($m['weight'])) * $c;
  $v2 = $v2 + (intval($m['weight']));
};
  return $v1 / toFloat($v2);
};
  function spaces($n) {
  global $house1, $house2, $h1_bedrooms, $h1_bathrooms, $h1_attic, $h1_kitchen, $h1_living_rooms, $h1_basement, $h1_garage, $h1_garden, $h2_upstairs, $h2_groundfloor, $h2_basement, $h1_bathroom1, $h1_bathroom2, $h1_outside, $h1_lounge, $h1_dining, $h1_conservatory, $h1_playroom, $h2_bedrooms, $h2_bathroom, $h2_toilet, $h2_attics, $h2_kitchen, $h2_living_rooms, $h2_wet_room, $h2_garage, $h2_garden, $h2_hot_tub, $h2_cellars, $h2_wine_cellar, $h2_cinema, $h2_suite1, $h2_suite2, $h2_bedroom3, $h2_bedroom4, $h2_lounge, $h2_dining, $h2_conservatory, $h2_playroom;
  return repeat(' ', $n);
};
  function show($n, $level) {
  global $house1, $house2, $h1_bedrooms, $h1_bathrooms, $h1_attic, $h1_kitchen, $h1_living_rooms, $h1_basement, $h1_garage, $h1_garden, $h2_upstairs, $h2_groundfloor, $h2_basement, $h1_bathroom1, $h1_bathroom2, $h1_outside, $h1_lounge, $h1_dining, $h1_conservatory, $h1_playroom, $h2_bedrooms, $h2_bathroom, $h2_toilet, $h2_attics, $h2_kitchen, $h2_living_rooms, $h2_wet_room, $h2_garage, $h2_garden, $h2_hot_tub, $h2_cellars, $h2_wine_cellar, $h2_cinema, $h2_suite1, $h2_suite2, $h2_bedroom3, $h2_bedroom4, $h2_lounge, $h2_dining, $h2_conservatory, $h2_playroom;
  $indent = $level * 4;
  $name = strval($n['name']);
  $nl = strlen($name) + $indent;
  $line = spaces($indent) . $name;
  $line = $line . spaces(32 - $nl) . '|  ';
  $line = $line . padLeft(_str(intval($n['weight'])), 3) . '   | ';
  $line = $line . formatFloat(computeCoverage($n), 6) . ' |';
  echo rtrim($line), PHP_EOL;
  $cs = $n['children'];
  foreach ($cs as $child) {
  show($child, $level + 1);
};
};
  $house1 = newNode('house1', 40, 0.0);
  $house2 = newNode('house2', 60, 0.0);
  $h1_bedrooms = newNode('bedrooms', 1, 0.25);
  $h1_bathrooms = newNode('bathrooms', 1, 0.0);
  $h1_attic = newNode('attic', 1, 0.75);
  $h1_kitchen = newNode('kitchen', 1, 0.1);
  $h1_living_rooms = newNode('living_rooms', 1, 0.0);
  $h1_basement = newNode('basement', 1, 0.0);
  $h1_garage = newNode('garage', 1, 0.0);
  $h1_garden = newNode('garden', 1, 0.8);
  $h2_upstairs = newNode('upstairs', 1, 0.0);
  $h2_groundfloor = newNode('groundfloor', 1, 0.0);
  $h2_basement = newNode('basement', 1, 0.0);
  $h1_bathroom1 = newNode('bathroom1', 1, 0.5);
  $h1_bathroom2 = newNode('bathroom2', 1, 0.0);
  $h1_outside = newNode('outside_lavatory', 1, 1.0);
  $h1_lounge = newNode('lounge', 1, 0.0);
  $h1_dining = newNode('dining_room', 1, 0.0);
  $h1_conservatory = newNode('conservatory', 1, 0.0);
  $h1_playroom = newNode('playroom', 1, 1.0);
  $h2_bedrooms = newNode('bedrooms', 1, 0.0);
  $h2_bathroom = newNode('bathroom', 1, 0.0);
  $h2_toilet = newNode('toilet', 1, 0.0);
  $h2_attics = newNode('attics', 1, 0.6);
  $h2_kitchen = newNode('kitchen', 1, 0.0);
  $h2_living_rooms = newNode('living_rooms', 1, 0.0);
  $h2_wet_room = newNode('wet_room_&_toilet', 1, 0.0);
  $h2_garage = newNode('garage', 1, 0.0);
  $h2_garden = newNode('garden', 1, 0.9);
  $h2_hot_tub = newNode('hot_tub_suite', 1, 1.0);
  $h2_cellars = newNode('cellars', 1, 1.0);
  $h2_wine_cellar = newNode('wine_cellar', 1, 1.0);
  $h2_cinema = newNode('cinema', 1, 0.75);
  $h2_suite1 = newNode('suite_1', 1, 0.0);
  $h2_suite2 = newNode('suite_2', 1, 0.0);
  $h2_bedroom3 = newNode('bedroom_3', 1, 0.0);
  $h2_bedroom4 = newNode('bedroom_4', 1, 0.0);
  $h2_lounge = newNode('lounge', 1, 0.0);
  $h2_dining = newNode('dining_room', 1, 0.0);
  $h2_conservatory = newNode('conservatory', 1, 0.0);
  $h2_playroom = newNode('playroom', 1, 0.0);
  function main() {
  global $house1, $house2, $h1_bedrooms, $h1_bathrooms, $h1_attic, $h1_kitchen, $h1_living_rooms, $h1_basement, $h1_garage, $h1_garden, $h2_upstairs, $h2_groundfloor, $h2_basement, $h1_bathroom1, $h1_bathroom2, $h1_outside, $h1_lounge, $h1_dining, $h1_conservatory, $h1_playroom, $h2_bedrooms, $h2_bathroom, $h2_toilet, $h2_attics, $h2_kitchen, $h2_living_rooms, $h2_wet_room, $h2_garage, $h2_garden, $h2_hot_tub, $h2_cellars, $h2_wine_cellar, $h2_cinema, $h2_suite1, $h2_suite2, $h2_bedroom3, $h2_bedroom4, $h2_lounge, $h2_dining, $h2_conservatory, $h2_playroom;
  $cleaning = newNode('cleaning', 1, 0.0);
  addChildren($h1_bathrooms, [$h1_bathroom1, $h1_bathroom2, $h1_outside]);
  addChildren($h1_living_rooms, [$h1_lounge, $h1_dining, $h1_conservatory, $h1_playroom]);
  addChildren($house1, [$h1_bedrooms, $h1_bathrooms, $h1_attic, $h1_kitchen, $h1_living_rooms, $h1_basement, $h1_garage, $h1_garden]);
  addChildren($h2_bedrooms, [$h2_suite1, $h2_suite2, $h2_bedroom3, $h2_bedroom4]);
  addChildren($h2_upstairs, [$h2_bedrooms, $h2_bathroom, $h2_toilet, $h2_attics]);
  addChildren($h2_living_rooms, [$h2_lounge, $h2_dining, $h2_conservatory, $h2_playroom]);
  addChildren($h2_groundfloor, [$h2_kitchen, $h2_living_rooms, $h2_wet_room, $h2_garage, $h2_garden, $h2_hot_tub]);
  addChildren($h2_basement, [$h2_cellars, $h2_wine_cellar, $h2_cinema]);
  addChildren($house2, [$h2_upstairs, $h2_groundfloor, $h2_basement]);
  addChildren($cleaning, [$house1, $house2]);
  $topCoverage = computeCoverage($cleaning);
  echo rtrim('TOP COVERAGE = ' . formatFloat($topCoverage, 6)), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  echo rtrim('NAME HIERARCHY                 | WEIGHT | COVERAGE |'), PHP_EOL;
  show($cleaning, 0);
  setCoverage($h2_cinema, 1.0);
  $diff = computeCoverage($cleaning) - $topCoverage;
  echo rtrim(''), PHP_EOL;
  echo rtrim('If the coverage of the Cinema node were increased from 0.75 to 1'), PHP_EOL;
  echo rtrim('the top level coverage would increase by ' . formatFloat($diff, 6) . ' to ' . formatFloat($topCoverage + $diff, 6)), PHP_EOL;
  setCoverage($h2_cinema, 0.75);
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
