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
function padLeft($n, $width) {
  global $strings, $chars, $i, $j, $c, $ss;
  $s = _str($n);
  while (strlen($s) < $width) {
  $s = ' ' . $s;
};
  return $s;
}
function squeeze($s, $ch) {
  global $strings, $chars, $j, $ss;
  $out = '';
  $prev = false;
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if ($c == $ch) {
  if (!$prev) {
  $out = $out . $c;
  $prev = true;
};
} else {
  $out = $out . $c;
  $prev = false;
}
  $i = $i + 1;
};
  return $out;
}
$strings = ['', '"If I were two-faced, would I be wearing this one?" --- Abraham Lincoln ', '..1111111111111111111111111111111111111111111111111111111111111117777888', 'I never give \'em hell, I just tell the truth, and they think it\'s hell. ', '                                                   ---  Harry S Truman  ', 'The better the 4-wheel drive, the further you\'ll be from help when ya get stuck!', 'headmistressship', 'aardvark', '😍😀🙌💃😍😍😍🙌'];
$chars = [[' '], ['-'], ['7'], ['.'], [' ', '-', 'r'], ['e'], ['s'], ['a'], ['😍']];
$i = 0;
while ($i < count($strings)) {
  $j = 0;
  $s = $strings[$i];
  while ($j < count($chars[$i])) {
  $c = $chars[$i][$j];
  $ss = squeeze($s, $c);
  echo rtrim('specified character = \'' . $c . '\''), PHP_EOL;
  echo rtrim('original : length = ' . padLeft(strlen($s), 2) . ', string = «««' . $s . '»»»'), PHP_EOL;
  echo rtrim('squeezed : length = ' . padLeft(strlen($ss), 2) . ', string = «««' . $ss . '»»»'), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  $j = $j + 1;
};
  $i = $i + 1;
}
