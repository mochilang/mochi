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
$__start_mem = memory_get_usage();
$__start = _now();
  function is_valid($puzzle, $word, $row, $col, $vertical) {
  global $words, $used;
  for ($i = 0; $i < strlen($word); $i++) {
  if ($vertical) {
  if ($row + $i >= count($puzzle) || $puzzle[$row + $i][$col] != '') {
  return false;
};
} else {
  if ($col + $i >= count($puzzle[0]) || $puzzle[$row][$col + $i] != '') {
  return false;
};
}
};
  return true;
};
  function place_word(&$puzzle, $word, $row, $col, $vertical) {
  global $words, $used;
  for ($i = 0; $i < strlen($word); $i++) {
  $ch = substr($word, $i, $i + 1 - $i);
  if ($vertical) {
  $puzzle[$row + $i][$col] = $ch;
} else {
  $puzzle[$row][$col + $i] = $ch;
}
};
};
  function remove_word(&$puzzle, $word, $row, $col, $vertical) {
  global $words, $used;
  for ($i = 0; $i < strlen($word); $i++) {
  if ($vertical) {
  $puzzle[$row + $i][$col] = '';
} else {
  $puzzle[$row][$col + $i] = '';
}
};
};
  function solve_crossword(&$puzzle, $words, &$used) {
  for ($row = 0; $row < count($puzzle); $row++) {
  for ($col = 0; $col < count($puzzle[0]); $col++) {
  if ($puzzle[$row][$col] == '') {
  for ($i = 0; $i < count($words); $i++) {
  if (!$used[$i]) {
  $word = $words[$i];
  foreach ([true, false] as $vertical) {
  if (is_valid($puzzle, $word, $row, $col, $vertical)) {
  place_word($puzzle, $word, $row, $col, $vertical);
  $used[$i] = true;
  if (solve_crossword($puzzle, $words, $used)) {
  return true;
};
  $used[$i] = false;
  remove_word($puzzle, $word, $row, $col, $vertical);
}
};
}
};
  return false;
}
};
};
  return true;
};
  $puzzle = [['', '', ''], ['', '', ''], ['', '', '']];
  $words = ['cat', 'dog', 'car'];
  $used = [false, false, false];
  if (solve_crossword($puzzle, $words, $used)) {
  echo rtrim('Solution found:'), PHP_EOL;
  foreach ($puzzle as $row) {
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($row, 1344))))))), PHP_EOL;
};
} else {
  echo rtrim('No solution found:'), PHP_EOL;
}
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
