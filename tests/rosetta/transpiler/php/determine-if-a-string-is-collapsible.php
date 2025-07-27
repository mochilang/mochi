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
function collapse($s) {
  $i = 0;
  $prev = '';
  $res = '';
  $orig = strlen($s);
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch != $prev) {
  $res = $res . $ch;
  $prev = $ch;
}
  $i = $i + 1;
};
  return [$res, $orig, strlen($res)];
}
function main() {
  $strings = ['', '"If I were two-faced, would I be wearing this one?" --- Abraham Lincoln ', '..111111111111111111111111111111111111111111111111111111111111111777888', 'I never give \'em hell, I just tell the truth, and they think it\'s hell. ', '                                                   ---  Harry S Truman ', 'The better the 4-wheel drive, the further you\'ll be from help when ya get stuck!', 'headmistressship', 'aardvark', '😍😀🙌💃😍😍😍🙌'];
  $idx = 0;
  while ($idx < count($strings)) {
  $s = $strings[$idx];
  $r = collapse($s);
  $cs = $r[0];
  $olen = $r[1];
  $clen = $r[2];
  echo rtrim('original : length = ' . _str($olen) . ', string = «««' . $s . '»»»'), PHP_EOL;
  echo rtrim('collapsed: length = ' . _str($clen) . ', string = «««' . $cs . '»»»
'), PHP_EOL;
  $idx = $idx + 1;
};
}
main();
