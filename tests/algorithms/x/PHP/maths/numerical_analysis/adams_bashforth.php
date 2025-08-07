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
  function abs_float($x) {
  global $y2, $y3, $y4, $y5;
  if ($x < 0.0) {
  return -$x;
} else {
  return $x;
}
};
  function validate_inputs($x_initials, $step_size, $x_final) {
  global $y2, $y3, $y4, $y5;
  if ($x_initials[count($x_initials) - 1] >= $x_final) {
  $panic('The final value of x must be greater than the initial values of x.');
}
  if ($step_size <= 0.0) {
  $panic('Step size must be positive.');
}
  $i = 0;
  while ($i < count($x_initials) - 1) {
  $diff = $x_initials[$i + 1] - $x_initials[$i];
  if (abs_float($diff - $step_size) > 0.0000000001) {
  $panic('x-values must be equally spaced according to step size.');
}
  $i = $i + 1;
};
};
  function list_to_string($xs) {
  global $y2, $y3, $y4, $y5;
  $s = '[';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . _str($xs[$i]);
  if ($i + 1 < count($xs)) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
};
  function adams_bashforth_step2($f, $x_initials, $y_initials, $step_size, $x_final) {
  global $y2, $y3, $y4, $y5;
  validate_inputs($x_initials, $step_size, $x_final);
  if (count($x_initials) != 2 || count($y_initials) != 2) {
  $panic('Insufficient initial points information.');
}
  $x0 = $x_initials[0];
  $x1 = $x_initials[1];
  $y = [];
  $y = _append($y, $y_initials[0]);
  $y = _append($y, $y_initials[1]);
  $n = intval((($x_final - $x1) / $step_size));
  $i = 0;
  while ($i < $n) {
  $term = 3.0 * $f($x1, $y[$i + 1]) - $f($x0, $y[$i]);
  $y_next = $y[$i + 1] + ($step_size / 2.0) * $term;
  $y = _append($y, $y_next);
  $x0 = $x1;
  $x1 = $x1 + $step_size;
  $i = $i + 1;
};
  return $y;
};
  function adams_bashforth_step3($f, $x_initials, $y_initials, $step_size, $x_final) {
  global $y2, $y3, $y4, $y5;
  validate_inputs($x_initials, $step_size, $x_final);
  if (count($x_initials) != 3 || count($y_initials) != 3) {
  $panic('Insufficient initial points information.');
}
  $x0 = $x_initials[0];
  $x1 = $x_initials[1];
  $x2 = $x_initials[2];
  $y = [];
  $y = _append($y, $y_initials[0]);
  $y = _append($y, $y_initials[1]);
  $y = _append($y, $y_initials[2]);
  $n = intval((($x_final - $x2) / $step_size));
  $i = 0;
  while ($i <= $n) {
  $term = 23.0 * $f($x2, $y[$i + 2]) - 16.0 * $f($x1, $y[$i + 1]) + 5.0 * $f($x0, $y[$i]);
  $y_next = $y[$i + 2] + ($step_size / 12.0) * $term;
  $y = _append($y, $y_next);
  $x0 = $x1;
  $x1 = $x2;
  $x2 = $x2 + $step_size;
  $i = $i + 1;
};
  return $y;
};
  function adams_bashforth_step4($f, $x_initials, $y_initials, $step_size, $x_final) {
  global $y2, $y3, $y4, $y5;
  validate_inputs($x_initials, $step_size, $x_final);
  if (count($x_initials) != 4 || count($y_initials) != 4) {
  $panic('Insufficient initial points information.');
}
  $x0 = $x_initials[0];
  $x1 = $x_initials[1];
  $x2 = $x_initials[2];
  $x3 = $x_initials[3];
  $y = [];
  $y = _append($y, $y_initials[0]);
  $y = _append($y, $y_initials[1]);
  $y = _append($y, $y_initials[2]);
  $y = _append($y, $y_initials[3]);
  $n = intval((($x_final - $x3) / $step_size));
  $i = 0;
  while ($i < $n) {
  $term = 55.0 * $f($x3, $y[$i + 3]) - 59.0 * $f($x2, $y[$i + 2]) + 37.0 * $f($x1, $y[$i + 1]) - 9.0 * $f($x0, $y[$i]);
  $y_next = $y[$i + 3] + ($step_size / 24.0) * $term;
  $y = _append($y, $y_next);
  $x0 = $x1;
  $x1 = $x2;
  $x2 = $x3;
  $x3 = $x3 + $step_size;
  $i = $i + 1;
};
  return $y;
};
  function adams_bashforth_step5($f, $x_initials, $y_initials, $step_size, $x_final) {
  global $y2, $y3, $y4, $y5;
  validate_inputs($x_initials, $step_size, $x_final);
  if (count($x_initials) != 5 || count($y_initials) != 5) {
  $panic('Insufficient initial points information.');
}
  $x0 = $x_initials[0];
  $x1 = $x_initials[1];
  $x2 = $x_initials[2];
  $x3 = $x_initials[3];
  $x4 = $x_initials[4];
  $y = [];
  $y = _append($y, $y_initials[0]);
  $y = _append($y, $y_initials[1]);
  $y = _append($y, $y_initials[2]);
  $y = _append($y, $y_initials[3]);
  $y = _append($y, $y_initials[4]);
  $n = intval((($x_final - $x4) / $step_size));
  $i = 0;
  while ($i <= $n) {
  $term = 1901.0 * $f($x4, $y[$i + 4]) - 2774.0 * $f($x3, $y[$i + 3]) - 2616.0 * $f($x2, $y[$i + 2]) - 1274.0 * $f($x1, $y[$i + 1]) + 251.0 * $f($x0, $y[$i]);
  $y_next = $y[$i + 4] + ($step_size / 720.0) * $term;
  $y = _append($y, $y_next);
  $x0 = $x1;
  $x1 = $x2;
  $x2 = $x3;
  $x3 = $x4;
  $x4 = $x4 + $step_size;
  $i = $i + 1;
};
  return $y;
};
  function f_x($x, $y) {
  global $y2, $y3, $y4, $y5;
  return $x;
};
  function f_xy($x, $y) {
  global $y2, $y3, $y4, $y5;
  return $x + $y;
};
  $y2 = adams_bashforth_step2('f_x', [0.0, 0.2], [0.0, 0.0], 0.2, 1.0);
  echo rtrim(list_to_string($y2)), PHP_EOL;
  $y3 = adams_bashforth_step3('f_xy', [0.0, 0.2, 0.4], [0.0, 0.0, 0.04], 0.2, 1.0);
  echo rtrim(_str($y3[3])), PHP_EOL;
  $y4 = adams_bashforth_step4('f_xy', [0.0, 0.2, 0.4, 0.6], [0.0, 0.0, 0.04, 0.128], 0.2, 1.0);
  echo rtrim(_str($y4[4])), PHP_EOL;
  echo rtrim(_str($y4[5])), PHP_EOL;
  $y5 = adams_bashforth_step5('f_xy', [0.0, 0.2, 0.4, 0.6, 0.8], [0.0, 0.0214, 0.0214, 0.22211, 0.42536], 0.2, 1.0);
  echo rtrim(_str($y5[count($y5) - 1])), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
