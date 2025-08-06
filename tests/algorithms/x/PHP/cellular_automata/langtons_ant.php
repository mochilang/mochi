<?php
ini_set('memory_limit', '-1');
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
function create_board($width, $height) {
  $board = [];
  $i = 0;
  while ($i < $height) {
  $row = [];
  $j = 0;
  while ($j < $width) {
  $row = _append($row, true);
  $j = $j + 1;
};
  $board = _append($board, $row);
  $i = $i + 1;
};
  return $board;
}
function move_ant(&$board, $x, $y, $direction) {
  if ($board[$x][$y]) {
  $direction = ($direction + 1) % 4;
} else {
  $direction = ($direction + 3) % 4;
}
  $old_x = $x;
  $old_y = $y;
  if ($direction == 0) {
  $x = $x - 1;
} else {
  if ($direction == 1) {
  $y = $y + 1;
} else {
  if ($direction == 2) {
  $x = $x + 1;
} else {
  $y = $y - 1;
};
};
}
  $board[$old_x][$old_y] = !$board[$old_x][$old_y];
  return [$x, $y, $direction];
}
function langtons_ant($width, $height, $steps) {
  $board = create_board($width, $height);
  $x = _intdiv($width, 2);
  $y = _intdiv($height, 2);
  $dir = 3;
  $s = 0;
  while ($s < $steps) {
  $state = move_ant($board, $x, $y, $dir);
  $x = $state[0];
  $y = $state[1];
  $dir = $state[2];
  $s = $s + 1;
};
  return $board;
}
