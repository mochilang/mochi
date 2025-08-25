<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        $q = bcdiv($sa, $sb, 0);
        $rem = bcmod($sa, $sb);
        $neg = ((strpos($sa, '-') === 0) xor (strpos($sb, '-') === 0));
        if ($neg && bccomp($rem, '0') != 0) {
            $q = bcsub($q, '1');
        }
        return intval($q);
    }
    $ai = intval($a);
    $bi = intval($b);
    $q = intdiv($ai, $bi);
    if ((($ai ^ $bi) < 0) && ($ai % $bi != 0)) {
        $q -= 1;
    }
    return $q;
}
$NUM_SQUARES = 9;
$EMPTY_CELL = '.';
function is_valid_sudoku_board($board) {
  global $EMPTY_CELL, $NUM_SQUARES, $invalid_board, $valid_board;
  if (count($board) != $NUM_SQUARES) {
  return false;
}
  $i = 0;
  while ($i < $NUM_SQUARES) {
  if (count($board[$i]) != $NUM_SQUARES) {
  return false;
}
  $i = $i + 1;
};
  $rows = [];
  $cols = [];
  $boxes = [];
  $i = 0;
  while ($i < $NUM_SQUARES) {
  $rows = _append($rows, []);
  $cols = _append($cols, []);
  $boxes = _append($boxes, []);
  $i = $i + 1;
};
  for ($r = 0; $r < $NUM_SQUARES; $r++) {
  for ($c = 0; $c < $NUM_SQUARES; $c++) {
  $value = $board[$r][$c];
  if ($value == $EMPTY_CELL) {
  continue;
}
  $box = intval(_intdiv($r, 3)) * 3 + intval(_intdiv($c, 3));
  if (in_array($value, $rows[$r]) || in_array($value, $cols[$c]) || in_array($value, $boxes[$box])) {
  return false;
}
  $rows[$r] = _append($rows[$r], $value);
  $cols[$c] = _append($cols[$c], $value);
  $boxes[$box] = _append($boxes[$box], $value);
};
};
  return true;
}
$valid_board = [['5', '3', '.', '.', '7', '.', '.', '.', '.'], ['6', '.', '.', '1', '9', '5', '.', '.', '.'], ['.', '9', '8', '.', '.', '.', '.', '6', '.'], ['8', '.', '.', '.', '6', '.', '.', '.', '3'], ['4', '.', '.', '8', '.', '3', '.', '.', '1'], ['7', '.', '.', '.', '2', '.', '.', '.', '6'], ['.', '6', '.', '.', '.', '.', '2', '8', '.'], ['.', '.', '.', '4', '1', '9', '.', '.', '5'], ['.', '.', '.', '.', '8', '.', '.', '7', '9']];
$invalid_board = [['8', '3', '.', '.', '7', '.', '.', '.', '.'], ['6', '.', '.', '1', '9', '5', '.', '.', '.'], ['.', '9', '8', '.', '.', '.', '.', '6', '.'], ['8', '.', '.', '.', '6', '.', '.', '.', '3'], ['4', '.', '.', '8', '.', '3', '.', '.', '1'], ['7', '.', '.', '.', '2', '.', '.', '.', '6'], ['.', '6', '.', '.', '.', '.', '2', '8', '.'], ['.', '.', '.', '4', '1', '9', '.', '.', '5'], ['.', '.', '.', '.', '8', '.', '.', '7', '9']];
echo rtrim(json_encode(is_valid_sudoku_board($valid_board), 1344)), PHP_EOL;
echo rtrim(json_encode(is_valid_sudoku_board($invalid_board), 1344)), PHP_EOL;
