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
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
$__start_mem = memory_get_usage();
$__start = _now();
  $g2lMap = ['♜' => 'R', '♞' => 'N', '♝' => 'B', '♛' => 'Q', '♚' => 'K', '♖' => 'R', '♘' => 'N', '♗' => 'B', '♕' => 'Q', '♔' => 'K'];
  $names = ['R' => 'rook', 'N' => 'knight', 'B' => 'bishop', 'Q' => 'queen', 'K' => 'king'];
  $ntable = ['01' => 0, '02' => 1, '03' => 2, '04' => 3, '12' => 4, '13' => 5, '14' => 6, '23' => 7, '24' => 8, '34' => 9];
  function indexOf($s, $sub) {
  global $g2lMap, $names, $ntable;
  $i = 0;
  while ($i <= strlen($s) - strlen($sub)) {
  if (substr($s, $i, $i + strlen($sub) - $i) == $sub) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function removeChar($s, $ch) {
  global $g2lMap, $names, $ntable;
  $res = '';
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if ($c != $ch) {
  $res = $res . $c;
}
  $i = $i + 1;
};
  return $res;
};
  function g2l($pieces) {
  global $g2lMap, $names, $ntable;
  $res = '';
  $i = 0;
  while ($i < strlen($pieces)) {
  $ch = substr($pieces, $i, $i + 1 - $i);
  $res = $res . $g2lMap[$ch];
  $i = $i + 1;
};
  return $res;
};
  function countChar($s, $ch) {
  global $g2lMap, $names, $ntable;
  $c = 0;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  $c = $c + 1;
}
  $i = $i + 1;
};
  return $c;
};
  function spid($pieces) {
  global $g2lMap, $names, $ntable;
  $pieces = g2l($pieces);
  if (strlen($pieces) != 8) {
  return -1;
}
  foreach (['K', 'Q'] as $one) {
  if (countChar($pieces, $one) != 1) {
  return -1;
}
};
  foreach (['R', 'N', 'B'] as $two) {
  if (countChar($pieces, $two) != 2) {
  return -1;
}
};
  $r1 = _indexof($pieces, 'R');
  $r2 = _indexof(substr($pieces, $r1 + 1, strlen($pieces) - ($r1 + 1)), 'R') + $r1 + 1;
  $k = _indexof($pieces, 'K');
  if ($k < $r1 || $k > $r2) {
  return -1;
}
  $b1 = _indexof($pieces, 'B');
  $b2 = _indexof(substr($pieces, $b1 + 1, strlen($pieces) - ($b1 + 1)), 'B') + $b1 + 1;
  if (($b2 - $b1) % 2 == 0) {
  return -1;
}
  $piecesN = removeChar(removeChar($pieces, 'Q'), 'B');
  $n1 = _indexof($piecesN, 'N');
  $n2 = _indexof(substr($piecesN, $n1 + 1, strlen($piecesN) - ($n1 + 1)), 'N') + $n1 + 1;
  $N = $ntable[_str($n1) . _str($n2)];
  $piecesQ = removeChar($pieces, 'B');
  $Q = _indexof($piecesQ, 'Q');
  $D = _indexof('0246', _str($b1));
  $L = _indexof('1357', _str($b2));
  if ($D == (0 - 1)) {
  $D = _indexof('0246', _str($b2));
  $L = _indexof('1357', _str($b1));
}
  return 96 * $N + 16 * $Q + 4 * $D + $L;
};
  function main() {
  global $g2lMap, $names, $ntable;
  foreach (['♕♘♖♗♗♘♔♖', '♖♘♗♕♔♗♘♖', '♖♕♘♗♗♔♖♘', '♖♘♕♗♗♔♖♘'] as $pieces) {
  echo rtrim($pieces . ' or ' . g2l($pieces) . ' has SP-ID of ' . _str(spid($pieces))), PHP_EOL;
};
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
