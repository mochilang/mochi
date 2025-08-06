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
function make_2d($n) {
  $res = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $row = _append($row, 0);
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return $res;
}
function matrix_chain_order($arr) {
  $n = count($arr);
  $m = make_2d($n);
  $s = make_2d($n);
  $chain_length = 2;
  while ($chain_length < $n) {
  $a = 1;
  while ($a < $n - $chain_length + 1) {
  $b = $a + $chain_length - 1;
  $m[$a][$b] = 1000000000;
  $c = $a;
  while ($c < $b) {
  $cost = $m[$a][$c] + $m[$c + 1][$b] + $arr[$a - 1] * $arr[$c] * $arr[$b];
  if ($cost < $m[$a][$b]) {
  $m[$a][$b] = $cost;
  $s[$a][$b] = $c;
}
  $c = $c + 1;
};
  $a = $a + 1;
};
  $chain_length = $chain_length + 1;
};
  return ['matrix' => $m, 'solution' => $s];
}
function optimal_parenthesization($s, $i, $j) {
  if ($i == $j) {
  return 'A' . _str($i);
} else {
  $left = optimal_parenthesization($s, $i, $s[$i][$j]);
  $right = optimal_parenthesization($s, $s[$i][$j] + 1, $j);
  return '( ' . $left . ' ' . $right . ' )';
}
}
function main() {
  $arr = [30, 35, 15, 5, 10, 20, 25];
  $n = count($arr);
  $res = matrix_chain_order($arr);
  $m = $res['matrix'];
  $s = $res['solution'];
  echo rtrim('No. of Operation required: ' . _str($m[1][$n - 1])), PHP_EOL;
  $seq = optimal_parenthesization($s, 1, $n - 1);
  echo rtrim($seq), PHP_EOL;
}
main();
