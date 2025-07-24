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
$n = [];
function initN() {
  global $n, $draw, $numbers, $num, $thousands, $hundreds, $tens, $ones;
  $i = 0;
  while ($i < 15) {
  $row = [];
  $j = 0;
  while ($j < 11) {
  $row = array_merge($row, [' ']);
  $j = $j + 1;
};
  $row[5] = 'x';
  $n = array_merge($n, [$row]);
  $i = $i + 1;
};
}
function horiz($c1, $c2, $r) {
  global $n, $draw, $numbers, $num, $thousands, $hundreds, $tens, $ones;
  $c = $c1;
  while ($c <= $c2) {
  $n[$r][$c] = 'x';
  $c = $c + 1;
};
}
function verti($r1, $r2, $c) {
  global $n, $draw, $numbers, $num, $thousands, $hundreds, $tens, $ones;
  $r = $r1;
  while ($r <= $r2) {
  $n[$r][$c] = 'x';
  $r = $r + 1;
};
}
function diagd($c1, $c2, $r) {
  global $n, $draw, $numbers, $num, $thousands, $hundreds, $tens, $ones;
  $c = $c1;
  while ($c <= $c2) {
  $n[$r + $c - $c1][$c] = 'x';
  $c = $c + 1;
};
}
function diagu($c1, $c2, $r) {
  global $n, $draw, $numbers, $num, $thousands, $hundreds, $tens, $ones;
  $c = $c1;
  while ($c <= $c2) {
  $n[$r - $c + $c1][$c] = 'x';
  $c = $c + 1;
};
}
$draw = [];
function initDraw() {
  global $n, $draw, $numbers, $num, $thousands, $hundreds, $tens, $ones;
  $draw[1] = function() use (&$n, &$draw) {
  horiz(6, 10, 0);
};
  $draw[2] = function() use (&$n, &$draw) {
  horiz(6, 10, 4);
};
  $draw[3] = function() use (&$n, &$draw) {
  diagd(6, 10, 0);
};
  $draw[4] = function() use (&$n, &$draw) {
  diagu(6, 10, 4);
};
  $draw[5] = function() use (&$n, &$draw) {
  call_user_func($draw[1]);
  call_user_func($draw[4]);
};
  $draw[6] = function() use (&$n, &$draw) {
  verti(0, 4, 10);
};
  $draw[7] = function() use (&$n, &$draw) {
  call_user_func($draw[1]);
  call_user_func($draw[6]);
};
  $draw[8] = function() use (&$n, &$draw) {
  call_user_func($draw[2]);
  call_user_func($draw[6]);
};
  $draw[9] = function() use (&$n, &$draw) {
  call_user_func($draw[1]);
  call_user_func($draw[8]);
};
  $draw[10] = function() use (&$n, &$draw) {
  horiz(0, 4, 0);
};
  $draw[20] = function() use (&$n, &$draw) {
  horiz(0, 4, 4);
};
  $draw[30] = function() use (&$n, &$draw) {
  diagu(0, 4, 4);
};
  $draw[40] = function() use (&$n, &$draw) {
  diagd(0, 4, 0);
};
  $draw[50] = function() use (&$n, &$draw) {
  call_user_func($draw[10]);
  call_user_func($draw[40]);
};
  $draw[60] = function() use (&$n, &$draw) {
  verti(0, 4, 0);
};
  $draw[70] = function() use (&$n, &$draw) {
  call_user_func($draw[10]);
  call_user_func($draw[60]);
};
  $draw[80] = function() use (&$n, &$draw) {
  call_user_func($draw[20]);
  call_user_func($draw[60]);
};
  $draw[90] = function() use (&$n, &$draw) {
  call_user_func($draw[10]);
  call_user_func($draw[80]);
};
  $draw[100] = function() use (&$n, &$draw) {
  horiz(6, 10, 14);
};
  $draw[200] = function() use (&$n, &$draw) {
  horiz(6, 10, 10);
};
  $draw[300] = function() use (&$n, &$draw) {
  diagu(6, 10, 14);
};
  $draw[400] = function() use (&$n, &$draw) {
  diagd(6, 10, 10);
};
  $draw[500] = function() use (&$n, &$draw) {
  call_user_func($draw[100]);
  call_user_func($draw[400]);
};
  $draw[600] = function() use (&$n, &$draw) {
  verti(10, 14, 10);
};
  $draw[700] = function() use (&$n, &$draw) {
  call_user_func($draw[100]);
  call_user_func($draw[600]);
};
  $draw[800] = function() use (&$n, &$draw) {
  call_user_func($draw[200]);
  call_user_func($draw[600]);
};
  $draw[900] = function() use (&$n, &$draw) {
  call_user_func($draw[100]);
  call_user_func($draw[800]);
};
  $draw[1000] = function() use (&$n, &$draw) {
  horiz(0, 4, 14);
};
  $draw[2000] = function() use (&$n, &$draw) {
  horiz(0, 4, 10);
};
  $draw[3000] = function() use (&$n, &$draw) {
  diagd(0, 4, 10);
};
  $draw[4000] = function() use (&$n, &$draw) {
  diagu(0, 4, 14);
};
  $draw[5000] = function() use (&$n, &$draw) {
  call_user_func($draw[1000]);
  call_user_func($draw[4000]);
};
  $draw[6000] = function() use (&$n, &$draw) {
  verti(10, 14, 0);
};
  $draw[7000] = function() use (&$n, &$draw) {
  call_user_func($draw[1000]);
  call_user_func($draw[6000]);
};
  $draw[8000] = function() use (&$n, &$draw) {
  call_user_func($draw[2000]);
  call_user_func($draw[6000]);
};
  $draw[9000] = function() use (&$n, &$draw) {
  call_user_func($draw[1000]);
  call_user_func($draw[8000]);
};
}
function printNumeral() {
  global $n, $draw, $numbers, $num, $thousands, $hundreds, $tens, $ones;
  $i = 0;
  while ($i < 15) {
  $line = '';
  $j = 0;
  while ($j < 11) {
  $line = $line . $n[$i][$j] . ' ';
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
};
  echo rtrim(''), PHP_EOL;
}
initDraw();
$numbers = [0, 1, 20, 300, 4000, 5555, 6789, 9999];
foreach ($numbers as $number) {
  initN();
  echo rtrim(_str($number) . ':'), PHP_EOL;
  $num = $number;
  $thousands = intdiv($num, 1000);
  $num = $num % 1000;
  $hundreds = intdiv($num, 100);
  $num = $num % 100;
  $tens = intdiv($num, 10);
  $ones = $num % 10;
  if ($thousands > 0) {
  call_user_func($draw[$thousands * 1000]);
}
  if ($hundreds > 0) {
  call_user_func($draw[$hundreds * 100]);
}
  if ($tens > 0) {
  call_user_func($draw[$tens * 10]);
}
  if ($ones > 0) {
  call_user_func($draw[$ones]);
}
  printNumeral();
}
