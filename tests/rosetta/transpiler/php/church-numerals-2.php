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
function id($x) {
  return $x;
}
function compose($f, $g) {
  return function($x) use ($f, $g) {
  return $f($g($x));
};
}
function zero() {
  return function($f) {
  return 'id';
};
}
function one() {
  return 'id';
}
function succ($n) {
  return function($f) use ($n) {
  return compose($f, $n($f));
};
}
function plus($m, $n) {
  return function($f) use ($m, $n) {
  return compose($m($f), $n($f));
};
}
function mult($m, $n) {
  return compose($m, $n);
}
function mochi_exp($m, $n) {
  return $n($m);
}
function toInt($x) {
  $counter = 0;
  $fCounter = function($f) use (&$fCounter, $x, $counter) {
  $counter = $counter + 1;
  return $f;
};
  call_user_func($x($fCounter), 'id');
  return $counter;
}
function toStr($x) {
  $s = '';
  $fCounter = function($f) use (&$fCounter, $x, $s) {
  $s = $s . '|';
  return $f;
};
  call_user_func($x($fCounter), 'id');
  return $s;
}
function main() {
  echo rtrim('zero = ' . _str(toInt(zero()))), PHP_EOL;
  $onev = one();
  echo rtrim('one = ' . _str(toInt($onev))), PHP_EOL;
  $two = succ(succ(zero()));
  echo rtrim('two = ' . _str(toInt($two))), PHP_EOL;
  $three = plus($onev, $two);
  echo rtrim('three = ' . _str(toInt($three))), PHP_EOL;
  $four = mult($two, $two);
  echo rtrim('four = ' . _str(toInt($four))), PHP_EOL;
  $eight = mochi_exp($two, $three);
  echo rtrim('eight = ' . _str(toInt($eight))), PHP_EOL;
  echo rtrim('toStr(four) = ' . toStr($four)), PHP_EOL;
}
main();
