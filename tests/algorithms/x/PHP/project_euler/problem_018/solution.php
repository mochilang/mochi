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
function solution() {
  $triangle = [[75], [95, 64], [17, 47, 82], [18, 35, 87, 10], [20, 4, 82, 47, 65], [19, 1, 23, 75, 3, 34], [88, 2, 77, 73, 7, 63, 67], [99, 65, 4, 28, 6, 16, 70, 92], [41, 41, 26, 56, 83, 40, 80, 70, 33], [41, 48, 72, 33, 47, 32, 37, 16, 94, 29], [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14], [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57], [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48], [63, 66, 4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31], [4, 62, 98, 27, 23, 9, 70, 98, 73, 93, 38, 53, 60, 4, 23]];
  $i = 1;
  while ($i < count($triangle)) {
  $j = 0;
  while ($j < count($triangle[$i])) {
  $prev_row = $triangle[$i - 1];
  $number1 = ($j != count($prev_row) ? $prev_row[$j] : 0);
  $number2 = ($j > 0 ? $prev_row[$j - 1] : 0);
  $max_val = ($number1 > $number2 ? $number1 : $number2);
  $triangle[$i][$j] = $triangle[$i][$j] + $max_val;
  $j = $j + 1;
};
  $i = $i + 1;
};
  $last = $triangle[count($triangle) - 1];
  $k = 0;
  $best = 0;
  while ($k < count($last)) {
  if ($last[$k] > $best) {
  $best = $last[$k];
}
  $k = $k + 1;
};
  return $best;
}
echo rtrim(_str(solution())), PHP_EOL;
