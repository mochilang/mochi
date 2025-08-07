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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function is_alnum($ch) {
  return ($ch >= '0' && $ch <= '9') || ($ch >= 'A' && $ch <= 'Z') || ($ch >= 'a' && $ch <= 'z');
};
  function to_int($token) {
  $res = 0;
  $i = 0;
  while ($i < strlen($token)) {
  $res = $res * 10 + (ord(substr($token, $i, $i + 1 - $i)));
  $i = $i + 1;
};
  return $res;
};
  function split($s, $sep) {
  $res = [];
  $current = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == $sep) {
  $res = _append($res, $current);
  $current = '';
} else {
  $current = $current . $ch;
}
  $i = $i + 1;
};
  $res = _append($res, $current);
  return $res;
};
  function parse_moves($input_str) {
  $pairs = explode(',', $input_str);
  $moves = [];
  $i = 0;
  while ($i < count($pairs)) {
  $pair = $pairs[$i];
  $numbers = [];
  $num = '';
  $j = 0;
  while ($j < strlen($pair)) {
  $ch = substr($pair, $j, $j + 1 - $j);
  if ($ch == ' ') {
  if ($num != '') {
  $numbers = _append($numbers, $num);
  $num = '';
};
} else {
  $num = $num . $ch;
}
  $j = $j + 1;
};
  if ($num != '') {
  $numbers = _append($numbers, $num);
}
  if (count($numbers) != 2) {
  _panic('Each move must have exactly two numbers.');
}
  $x = to_int($numbers[0]);
  $y = to_int($numbers[1]);
  $moves = _append($moves, [$x => $x, $y => $y]);
  $i = $i + 1;
};
  return $moves;
};
  function validate_matrix_size($size) {
  if ($size <= 0) {
  _panic('Matrix size must be a positive integer.');
}
};
  function validate_matrix_content($matrix, $size) {
  if (count($matrix) != $size) {
  _panic('The matrix dont match with size.');
}
  $i = 0;
  while ($i < $size) {
  $row = $matrix[$i];
  if (strlen($row) != $size) {
  _panic('Each row in the matrix must have exactly ' . _str($size) . ' characters.');
}
  $j = 0;
  while ($j < $size) {
  $ch = substr($row, $j, $j + 1 - $j);
  if (!is_alnum($ch)) {
  _panic('Matrix rows can only contain letters and numbers.');
}
  $j = $j + 1;
};
  $i = $i + 1;
};
};
  function validate_moves($moves, $size) {
  $i = 0;
  while ($i < count($moves)) {
  $mv = $moves[$i];
  if ($mv['x'] < 0 || $mv['x'] >= $size || $mv['y'] < 0 || $mv['y'] >= $size) {
  _panic('Move is out of bounds for a matrix.');
}
  $i = $i + 1;
};
};
  function contains($pos, $r, $c) {
  $i = 0;
  while ($i < count($pos)) {
  $p = $pos[$i];
  if ($p['x'] == $r && $p['y'] == $c) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function find_repeat($matrix_g, $row, $column, $size) {
  $column = $size - 1 - $column;
  $visited = [];
  $repeated = [];
  $color = $matrix_g[$column][$row];
  if ($color == '-') {
  return $repeated;
}
  $stack = [['x' => $column, 'y' => $row]];
  while (count($stack) > 0) {
  $idx = count($stack) - 1;
  $pos = $stack[$idx];
  $stack = array_slice($stack, 0, $idx - 0);
  if ($pos['x'] < 0 || $pos['x'] >= $size || $pos['y'] < 0 || $pos['y'] >= $size) {
  continue;
}
  if (contains($visited, $pos['x'], $pos['y'])) {
  continue;
}
  $visited = _append($visited, $pos);
  if ($matrix_g[$pos['x']][$pos['y']] == $color) {
  $repeated = _append($repeated, $pos);
  $stack = _append($stack, ['x' => $pos['x'] - 1, 'y' => $pos['y']]);
  $stack = _append($stack, ['x' => $pos['x'] + 1, 'y' => $pos['y']]);
  $stack = _append($stack, ['x' => $pos['x'], 'y' => $pos['y'] - 1]);
  $stack = _append($stack, ['x' => $pos['x'], 'y' => $pos['y'] + 1]);
}
};
  return $repeated;
};
  function increment_score($count) {
  return _intdiv($count * ($count + 1), 2);
};
  function move_x(&$matrix_g, $column, $size) {
  $new_list = [];
  $row = 0;
  while ($row < $size) {
  $val = $matrix_g[$row][$column];
  if ($val != '-') {
  $new_list = _append($new_list, $val);
} else {
  $new_list = array_merge([$val], $new_list);
}
  $row = $row + 1;
};
  $row = 0;
  while ($row < $size) {
  $matrix_g[$row][$column] = $new_list[$row];
  $row = $row + 1;
};
  return $matrix_g;
};
  function move_y(&$matrix_g, $size) {
  $empty_cols = [];
  $column = $size - 1;
  while ($column >= 0) {
  $row = 0;
  $all_empty = true;
  while ($row < $size) {
  if ($matrix_g[$row][$column] != '-') {
  $all_empty = false;
  break;
}
  $row = $row + 1;
};
  if ($all_empty) {
  $empty_cols = _append($empty_cols, $column);
}
  $column = $column - 1;
};
  $i = 0;
  while ($i < count($empty_cols)) {
  $col = $empty_cols[$i];
  $c = $col + 1;
  while ($c < $size) {
  $r = 0;
  while ($r < $size) {
  $matrix_g[$r][$c - 1] = $matrix_g[$r][$c];
  $r = $r + 1;
};
  $c = $c + 1;
};
  $r = 0;
  while ($r < $size) {
  $matrix_g[$r][$size - 1] = '-';
  $r = $r + 1;
};
  $i = $i + 1;
};
  return $matrix_g;
};
  function play(&$matrix_g, $pos_x, $pos_y, $size) {
  $same_colors = find_repeat($matrix_g, $pos_x, $pos_y, $size);
  if (count($same_colors) != 0) {
  $i = 0;
  while ($i < count($same_colors)) {
  $p = $same_colors[$i];
  $matrix_g[$p['x']][$p['y']] = '-';
  $i = $i + 1;
};
  $column = 0;
  while ($column < $size) {
  $matrix_g = move_x($matrix_g, $column, $size);
  $column = $column + 1;
};
  $matrix_g = move_y($matrix_g, $size);
}
  $sc = increment_score(count($same_colors));
  return ['matrix' => $matrix_g, 'score' => $sc];
};
  function build_matrix($matrix) {
  $res = [];
  $i = 0;
  while ($i < count($matrix)) {
  $row = $matrix[$i];
  $row_list = [];
  $j = 0;
  while ($j < strlen($row)) {
  $row_list = _append($row_list, $row[$j]);
  $j = $j + 1;
};
  $res = _append($res, $row_list);
  $i = $i + 1;
};
  return $res;
};
  function process_game($size, $matrix, $moves) {
  $game_matrix = build_matrix($matrix);
  $total = 0;
  $i = 0;
  while ($i < count($moves)) {
  $mv = $moves[$i];
  $res = play($game_matrix, $mv['x'], $mv['y'], $size);
  $game_matrix = $res['matrix'];
  $total = $total + $res['score'];
  $i = $i + 1;
};
  return $total;
};
  function main() {
  $size = 4;
  $matrix = ['RRBG', 'RBBG', 'YYGG', 'XYGG'];
  $moves = parse_moves('0 1,1 1');
  validate_matrix_size($size);
  validate_matrix_content($matrix, $size);
  validate_moves($moves, $size);
  $score = process_game($size, $matrix, $moves);
  echo rtrim(_str($score)), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
