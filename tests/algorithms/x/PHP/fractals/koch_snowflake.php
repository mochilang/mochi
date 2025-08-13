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
$__start_mem = memory_get_usage();
$__start = _now();
  $PI = 3.141592653589793;
  $TWO_PI = 6.283185307179586;
  function _mod($x, $m) {
  global $INITIAL_VECTORS, $PI, $TWO_PI, $VECTOR_1, $VECTOR_2, $VECTOR_3, $example;
  return $x - (floatval(intval($x / $m))) * $m;
};
  function mochi_sin($x) {
  global $INITIAL_VECTORS, $PI, $TWO_PI, $VECTOR_1, $VECTOR_2, $VECTOR_3, $example;
  $y = _mod($x + $PI, $TWO_PI) - $PI;
  $y2 = $y * $y;
  $y3 = $y2 * $y;
  $y5 = $y3 * $y2;
  $y7 = $y5 * $y2;
  return $y - $y3 / 6.0 + $y5 / 120.0 - $y7 / 5040.0;
};
  function mochi_cos($x) {
  global $INITIAL_VECTORS, $PI, $TWO_PI, $VECTOR_1, $VECTOR_2, $VECTOR_3, $example;
  $y = _mod($x + $PI, $TWO_PI) - $PI;
  $y2 = $y * $y;
  $y4 = $y2 * $y2;
  $y6 = $y4 * $y2;
  return 1.0 - $y2 / 2.0 + $y4 / 24.0 - $y6 / 720.0;
};
  function rotate($v, $angle_deg) {
  global $INITIAL_VECTORS, $PI, $TWO_PI, $VECTOR_1, $VECTOR_2, $VECTOR_3, $example;
  $theta = $angle_deg * $PI / 180.0;
  $c = mochi_cos($theta);
  $s = mochi_sin($theta);
  return ['x' => $v['x'] * $c - $v['y'] * $s, 'y' => $v['x'] * $s + $v['y'] * $c];
};
  function iteration_step($vectors) {
  global $INITIAL_VECTORS, $PI, $TWO_PI, $VECTOR_1, $VECTOR_2, $VECTOR_3, $example;
  $new_vectors = [];
  $i = 0;
  while ($i < count($vectors) - 1) {
  $start = $vectors[$i];
  $end = $vectors[$i + 1];
  $new_vectors = _append($new_vectors, $start);
  $dx = $end['x'] - $start['x'];
  $dy = $end['y'] - $start['y'];
  $one_third = ['x' => $start['x'] + $dx / 3.0, 'y' => $start['y'] + $dy / 3.0];
  $mid = rotate(['x' => $dx / 3.0, 'y' => $dy / 3.0], 60.0);
  $peak = ['x' => $one_third['x'] + $mid['x'], 'y' => $one_third['y'] + $mid['y']];
  $two_third = ['x' => $start['x'] + $dx * 2.0 / 3.0, 'y' => $start['y'] + $dy * 2.0 / 3.0];
  $new_vectors = _append($new_vectors, $one_third);
  $new_vectors = _append($new_vectors, $peak);
  $new_vectors = _append($new_vectors, $two_third);
  $i = $i + 1;
};
  $new_vectors = _append($new_vectors, $vectors[count($vectors) - 1]);
  return $new_vectors;
};
  function iterate($initial, $steps) {
  global $INITIAL_VECTORS, $PI, $TWO_PI, $VECTOR_1, $VECTOR_2, $VECTOR_3, $example;
  $vectors = $initial;
  $i = 0;
  while ($i < $steps) {
  $vectors = iteration_step($vectors);
  $i = $i + 1;
};
  return $vectors;
};
  function vec_to_string($v) {
  global $INITIAL_VECTORS, $PI, $TWO_PI, $VECTOR_1, $VECTOR_2, $VECTOR_3, $example;
  return '(' . _str($v['x']) . ', ' . _str($v['y']) . ')';
};
  function vec_list_to_string($lst) {
  global $INITIAL_VECTORS, $PI, $TWO_PI, $VECTOR_1, $VECTOR_2, $VECTOR_3, $example;
  $res = '[';
  $i = 0;
  while ($i < count($lst)) {
  $res = $res . vec_to_string($lst[$i]);
  if ($i < count($lst) - 1) {
  $res = $res . ', ';
}
  $i = $i + 1;
};
  $res = $res . ']';
  return $res;
};
  $VECTOR_1 = ['x' => 0.0, 'y' => 0.0];
  $VECTOR_2 = ['x' => 0.5, 'y' => 0.8660254];
  $VECTOR_3 = ['x' => 1.0, 'y' => 0.0];
  $INITIAL_VECTORS = [$VECTOR_1, $VECTOR_2, $VECTOR_3, $VECTOR_1];
  $example = iterate([$VECTOR_1, $VECTOR_3], 1);
  echo rtrim(vec_list_to_string($example)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
