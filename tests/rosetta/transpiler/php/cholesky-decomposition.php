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
function sqrtApprox($x) {
  global $cholesky, $printMat, $demo;
  $guess = $x;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
}
function cholesky($a) {
  global $sqrtApprox, $printMat, $demo;
  $n = count($a);
  $l = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $row = array_merge($row, [0.0]);
  $j = $j + 1;
};
  $l = array_merge($l, [$row]);
  $i = $i + 1;
};
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j <= $i) {
  $sum = $a[$i][$j];
  $k = 0;
  while ($k < $j) {
  $sum = $sum - $l[$i][$k] * $l[$j][$k];
  $k = $k + 1;
};
  if ($i == $j) {
  $l[$i][$j] = sqrtApprox($sum);
} else {
  $l[$i][$j] = $sum / $l[$j][$j];
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $l;
}
function printMat($m) {
  global $sqrtApprox, $cholesky, $demo;
  $i = 0;
  while ($i < count($m)) {
  $line = '';
  $j = 0;
  while ($j < count($m[$i])) {
  $line = $line . _str($m[$i][$j]);
  if ($j < count($m[$i]) - 1) {
  $line = $line . ' ';
}
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
};
}
function demo($a) {
  global $sqrtApprox, $cholesky, $printMat;
  echo rtrim('A:'), PHP_EOL;
  printMat($a);
  $l = cholesky($a);
  echo rtrim('L:'), PHP_EOL;
  printMat($l);
}
demo([[25.0, 15.0, -5.0], [15.0, 18.0, 0.0], [-5.0, 0.0, 11.0]]);
demo([[18.0, 22.0, 54.0, 42.0], [22.0, 70.0, 86.0, 62.0], [54.0, 86.0, 174.0, 134.0], [42.0, 62.0, 134.0, 106.0]]);
