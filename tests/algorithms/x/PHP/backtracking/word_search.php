<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function contains($xs, $x) {
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  return true;
}
  $i = $i + 1;
};
  return false;
}
function get_point_key($len_board, $len_board_column, $row, $column) {
  return $len_board * $len_board_column * $row + $column;
}
function search_from($board, $word, $row, $column, $word_index, $visited) {
  if ($board[$row][$column] != substr($word, $word_index, $word_index + 1 - $word_index)) {
  return false;
}
  if ($word_index == strlen($word) - 1) {
  return true;
}
  $len_board = count($board);
  $len_board_column = count($board[0]);
  $dir_i = [0, 0, -1, 1];
  $dir_j = [1, -1, 0, 0];
  $k = 0;
  while ($k < 4) {
  $next_i = $row + $dir_i[$k];
  $next_j = $column + $dir_j[$k];
  if (!(0 <= $next_i && $next_i < $len_board && 0 <= $next_j && $next_j < $len_board_column)) {
  $k = $k + 1;
  continue;
}
  $key = get_point_key($len_board, $len_board_column, $next_i, $next_j);
  if (in_array($key, $visited)) {
  $k = $k + 1;
  continue;
}
  $new_visited = _append($visited, $key);
  if (search_from($board, $word, $next_i, $next_j, $word_index + 1, $new_visited)) {
  return true;
}
  $k = $k + 1;
};
  return false;
}
function word_exists($board, $word) {
  $len_board = count($board);
  $len_board_column = count($board[0]);
  $i = 0;
  while ($i < $len_board) {
  $j = 0;
  while ($j < $len_board_column) {
  $key = get_point_key($len_board, $len_board_column, $i, $j);
  $visited = _append([], $key);
  if (search_from($board, $word, $i, $j, 0, $visited)) {
  return true;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return false;
}
function main() {
  $board = [['A', 'B', 'C', 'E'], ['S', 'F', 'C', 'S'], ['A', 'D', 'E', 'E']];
  echo rtrim(json_encode(word_exists($board, 'ABCCED'), 1344)), PHP_EOL;
  echo rtrim(json_encode(word_exists($board, 'SEE'), 1344)), PHP_EOL;
  echo rtrim(json_encode(word_exists($board, 'ABCB'), 1344)), PHP_EOL;
}
main();
