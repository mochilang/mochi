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
function count_alive_neighbours($board, $row, $col) {
  global $glider, $i;
  $size = count($board);
  $alive = 0;
  $dr = -1;
  while ($dr < 2) {
  $dc = -1;
  while ($dc < 2) {
  $nr = $row + $dr;
  $nc = $col + $dc;
  if (!($dr == 0 && $dc == 0) && $nr >= 0 && $nr < $size && $nc >= 0 && $nc < $size) {
  if ($board[$nr][$nc]) {
  $alive = $alive + 1;
};
}
  $dc = $dc + 1;
};
  $dr = $dr + 1;
};
  return $alive;
}
function next_state($current, $alive) {
  global $glider, $board, $i;
  $state = $current;
  if ($current) {
  if ($alive < 2) {
  $state = false;
} else {
  if ($alive == 2 || $alive == 3) {
  $state = true;
} else {
  $state = false;
};
};
} else {
  if ($alive == 3) {
  $state = true;
};
}
  return $state;
}
function step($board) {
  global $glider, $i;
  $size = count($board);
  $new_board = [];
  $r = 0;
  while ($r < $size) {
  $new_row = [];
  $c = 0;
  while ($c < $size) {
  $alive = count_alive_neighbours($board, $r, $c);
  $cell = $board[$r][$c];
  $updated = next_state($cell, $alive);
  $new_row = _append($new_row, $updated);
  $c = $c + 1;
};
  $new_board = _append($new_board, $new_row);
  $r = $r + 1;
};
  return $new_board;
}
function show($board) {
  global $glider, $i;
  $r = 0;
  while ($r < count($board)) {
  $line = '';
  $c = 0;
  while ($c < count($board[$r])) {
  if ($board[$r][$c]) {
  $line = $line . '#';
} else {
  $line = $line . '.';
}
  $c = $c + 1;
};
  echo rtrim($line), PHP_EOL;
  $r = $r + 1;
};
}
$glider = [[false, true, false, false, false], [false, false, true, false, false], [true, true, true, false, false], [false, false, false, false, false], [false, false, false, false, false]];
$board = $glider;
echo rtrim('Initial'), PHP_EOL;
show($board);
$i = 0;
while ($i < 4) {
  $board = step($board);
  echo rtrim('
Step ' . _str($i + 1)), PHP_EOL;
  show($board);
  $i = $i + 1;
}
