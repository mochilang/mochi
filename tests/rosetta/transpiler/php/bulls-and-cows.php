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
function indexOf($s, $ch) {
  global $mochi_shuffle, $main;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function mochi_shuffle($xs) {
  global $indexOf, $main;
  $arr = $xs;
  $i = count($arr) - 1;
  while ($i > 0) {
  $j = _now() % ($i + 1);
  $tmp = $arr[$i];
  $arr[$i] = $arr[$j];
  $arr[$j] = $tmp;
  $i = $i - 1;
};
  return $arr;
}
function main() {
  global $indexOf, $mochi_shuffle;
  echo rtrim('Cows and Bulls'), PHP_EOL;
  echo rtrim('Guess four digit number of unique digits in the range 1 to 9.'), PHP_EOL;
  echo rtrim('A correct digit but not in the correct place is a cow.'), PHP_EOL;
  echo rtrim('A correct digit in the correct place is a bull.'), PHP_EOL;
  $digits = ['1', '2', '3', '4', '5', '6', '7', '8', '9'];
  $digits = mochi_shuffle($digits);
  $pat = $digits[0] . $digits[1] . $digits[2] . $digits[3];
  $valid = '123456789';
  while (true) {
  echo rtrim('Guess: '), PHP_EOL;
  $guess = trim(fgets(STDIN));
  if (strlen($guess) != 4) {
  echo rtrim('Please guess a four digit number.'), PHP_EOL;
  continue;
}
  $cows = 0;
  $bulls = 0;
  $seen = '';
  $i = 0;
  $malformed = false;
  while ($i < 4) {
  $cg = substr($guess, $i, $i + 1 - $i);
  if (_indexof($seen, $cg) != (-1)) {
  echo rtrim('Repeated digit: ' . $cg), PHP_EOL;
  $malformed = true;
  break;
}
  $seen = $seen . $cg;
  $pos = _indexof($pat, $cg);
  if ($pos == (-1)) {
  if (_indexof($valid, $cg) == (-1)) {
  echo rtrim('Invalid digit: ' . $cg), PHP_EOL;
  $malformed = true;
  break;
};
} else {
  if ($pos == $i) {
  $bulls = $bulls + 1;
} else {
  $cows = $cows + 1;
};
}
  $i = $i + 1;
};
  if ($malformed) {
  continue;
}
  echo rtrim('Cows: ' . _str($cows) . ', bulls: ' . _str($bulls)), PHP_EOL;
  if ($bulls == 4) {
  echo rtrim('You got it.'), PHP_EOL;
  break;
}
};
}
main();
