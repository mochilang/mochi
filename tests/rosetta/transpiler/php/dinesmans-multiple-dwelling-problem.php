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
function absInt($n) {
  if ($n < 0) {
  return -$n;
}
  return $n;
}
function main() {
  $b = 1;
  while ($b <= 5) {
  if ($b != 5) {
  $c = 1;
  while ($c <= 5) {
  if ($c != 1 && $c != $b) {
  $f = 1;
  while ($f <= 5) {
  if ($f != 1 && $f != 5 && $f != $b && $f != $c && absInt($f - $c) > 1) {
  $m = 1;
  while ($m <= 5) {
  if ($m != $b && $m != $c && $m != $f && $m > $c) {
  $s = 1;
  while ($s <= 5) {
  if ($s != $b && $s != $c && $s != $f && $s != $m && absInt($s - $f) > 1) {
  echo rtrim('Baker in ' . _str($b) . ', Cooper in ' . _str($c) . ', Fletcher in ' . _str($f) . ', Miller in ' . _str($m) . ', Smith in ' . _str($s) . '.'), PHP_EOL;
  return;
}
  $s = $s + 1;
};
}
  $m = $m + 1;
};
}
  $f = $f + 1;
};
}
  $c = $c + 1;
};
}
  $b = $b + 1;
};
  echo rtrim('No solution found.'), PHP_EOL;
}
main();
