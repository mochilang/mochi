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
function parseIntStr($str) {
  global $splitWs, $parsePpm, $ppmData, $img;
  $i = 0;
  $neg = false;
  if (strlen($str) > 0 && substr($str, 0, 1 - 0) == '-') {
  $neg = true;
  $i = 1;
}
  $n = 0;
  $digits = ['0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9];
  while ($i < strlen($str)) {
  $n = $n * 10 + $digits[substr($str, $i, $i + 1 - $i)];
  $i = $i + 1;
};
  if ($neg) {
  $n = -$n;
}
  return $n;
}
function splitWs($s) {
  global $parseIntStr, $parsePpm, $ppmData, $img;
  $parts = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == ' ' || $ch == '
' || $ch == '	' || $ch == '') {
  if (strlen($cur) > 0) {
  $parts = array_merge($parts, [$cur]);
  $cur = '';
};
} else {
  $cur = $cur . $ch;
}
  $i = $i + 1;
};
  if (strlen($cur) > 0) {
  $parts = array_merge($parts, [$cur]);
}
  return $parts;
}
function parsePpm($data) {
  global $parseIntStr, $splitWs, $ppmData, $img;
  $toks = splitWs($data);
  if (count($toks) < 4) {
  return ['err' => true];
}
  $magic = $toks[0];
  $w = parseIntStr($toks[1]);
  $h = parseIntStr($toks[2]);
  $maxv = parseIntStr($toks[3]);
  $px = [];
  $i = 4;
  while ($i < count($toks)) {
  $px = array_merge($px, [parseIntStr($toks[$i])]);
  $i = $i + 1;
};
  return ['magic' => $magic, 'w' => $w, 'h' => $h, 'max' => $maxv, 'px' => $px];
}
$ppmData = 'P3
2 2
1
0 1 1 0 1 0 0 1 1 1 0 0
';
$img = parsePpm($ppmData);
echo rtrim('width=' . _str($img['w']) . ' height=' . _str($img['h'])), PHP_EOL;
