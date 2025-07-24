<?php
ini_set('memory_limit', '-1');
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
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
function parseIntStr($s, $base = 10) {
    return intval($s, intval($base));
}
function mochi_parseIntStr($str) {
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
function fields($s) {
  $words = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == ' ' || $ch == '	' || $ch == '
') {
  if (strlen($cur) > 0) {
  $words = array_merge($words, [$cur]);
  $cur = '';
};
} else {
  $cur = $cur . $ch;
}
  $i = $i + 1;
};
  if (strlen($cur) > 0) {
  $words = array_merge($words, [$cur]);
}
  return $words;
}
function unescape($s) {
  $out = '';
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == '\\' && $i + 1 < strlen($s)) {
  $c = substr($s, $i + 1, $i + 2 - $i + 1);
  if ($c == 'n') {
  $out = $out . '
';
  $i = $i + 2;
  continue;
} else {
  if ($c == '\\') {
  $out = $out . '\\';
  $i = $i + 2;
  continue;
};
};
}
  $out = $out . substr($s, $i, $i + 1 - $i);
  $i = $i + 1;
};
  return $out;
}
function parseProgram($src) {
  $lines = split($src, '
');
  $header = fields($lines[0]);
  $dataSize = parseIntStr($header[1], 10);
  $nStrings = parseIntStr($header[3], 10);
  $stringPool = [];
  $i = 1;
  while ($i <= $nStrings) {
  $s = $lines[$i];
  if (strlen($s) > 0) {
  $stringPool = array_merge($stringPool, [unescape(substr($s, 1, strlen($s) - 1 - 1))]);
}
  $i = $i + 1;
};
  $code = [];
  $addrMap = [];
  while ($i < count($lines)) {
  $line = mochi_trim($lines[$i]);
  if (strlen($line) == 0) {
  break;
}
  $parts = fields($line);
  $addr = parseIntStr($parts[0], 10);
  $op = $parts[1];
  $arg = 0;
  if ($op == 'push') {
  $arg = parseIntStr($parts[2], 10);
} else {
  if ($op == 'fetch' || $op == 'store') {
  $arg = parseIntStr(substr($parts[2], 1, strlen($parts[2]) - 1 - 1), 10);
} else {
  if ($op == 'jmp' || $op == 'jz') {
  $arg = parseIntStr($parts[3], 10);
};
};
}
  $code = array_merge($code, [['addr' => $addr, 'op' => $op, 'arg' => $arg]]);
  $addrMap[$addr] = count($code) - 1;
  $i = $i + 1;
};
  return ['dataSize' => $dataSize, 'strings' => $stringPool, 'code' => $code, 'addrMap' => $addrMap];
}
function runVM($prog) {
  $data = [];
  $i = 0;
  while ($i < $prog['dataSize']) {
  $data = array_merge($data, [0]);
  $i = $i + 1;
};
  $stack = [];
  $pc = 0;
  $code = $prog['code'];
  $addrMap = $prog['addrMap'];
  $pool = $prog['strings'];
  while ($pc < _len($code)) {
  $inst = $code[$pc];
  $op = $inst['op'];
  $arg = $inst['arg'];
  if ($op == 'push') {
  $stack = array_merge($stack, [$arg]);
  $pc = $pc + 1;
  continue;
}
  if ($op == 'store') {
  $data[$arg] = $stack[count($stack) - 1];
  $stack = array_slice($stack, 0, count($stack) - 1 - 0);
  $pc = $pc + 1;
  continue;
}
  if ($op == 'fetch') {
  $stack = array_merge($stack, [$data[$arg]]);
  $pc = $pc + 1;
  continue;
}
  if ($op == 'add') {
  $stack[count($stack) - 2] = $stack[count($stack) - 2] + $stack[count($stack) - 1];
  $stack = array_slice($stack, 0, count($stack) - 1 - 0);
  $pc = $pc + 1;
  continue;
}
  if ($op == 'lt') {
  $v = 0;
  if ($stack[count($stack) - 2] < $stack[count($stack) - 1]) {
  $v = 1;
};
  $stack[count($stack) - 2] = $v;
  $stack = array_slice($stack, 0, count($stack) - 1 - 0);
  $pc = $pc + 1;
  continue;
}
  if ($op == 'jz') {
  $v = $stack[count($stack) - 1];
  $stack = array_slice($stack, 0, count($stack) - 1 - 0);
  if ($v == 0) {
  $pc = $addrMap[$arg];
} else {
  $pc = $pc + 1;
};
  continue;
}
  if ($op == 'jmp') {
  $pc = $addrMap[$arg];
  continue;
}
  if ($op == 'prts') {
  echo rtrim(json_encode($pool[$stack[count($stack) - 1]], 1344)), PHP_EOL;
  $stack = array_slice($stack, 0, count($stack) - 1 - 0);
  $pc = $pc + 1;
  continue;
}
  if ($op == 'prti') {
  echo rtrim(json_encode(_str($stack[count($stack) - 1]), 1344)), PHP_EOL;
  $stack = array_slice($stack, 0, count($stack) - 1 - 0);
  $pc = $pc + 1;
  continue;
}
  if ($op == 'halt') {
  break;
}
  $pc = $pc + 1;
};
}
function mochi_trim($s) {
  $start = 0;
  while ($start < strlen($s) && (substr($s, $start, $start + 1 - $start) == ' ' || substr($s, $start, $start + 1 - $start) == '	')) {
  $start = $start + 1;
};
  $end = strlen($s);
  while ($end > $start && (substr($s, $end - 1, $end - $end - 1) == ' ' || substr($s, $end - 1, $end - $end - 1) == '	')) {
  $end = $end - 1;
};
  return substr($s, $start, $end - $start);
}
function split($s, $sep) {
  $parts = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($s)) {
  if (strlen($sep) > 0 && $i + strlen($sep) <= strlen($s) && substr($s, $i, $i + strlen($sep) - $i) == $sep) {
  $parts = array_merge($parts, [$cur]);
  $cur = '';
  $i = $i + strlen($sep);
} else {
  $cur = $cur . substr($s, $i, $i + 1 - $i);
  $i = $i + 1;
}
};
  $parts = array_merge($parts, [$cur]);
  return $parts;
}
function main() {
  $programText = 'Datasize: 1 Strings: 2
' . '"count is: "
' . '"\\n"
' . '    0 push  1
' . '    5 store [0]
' . '   10 fetch [0]
' . '   15 push  10
' . '   20 lt
' . '   21 jz     (43) 65
' . '   26 push  0
' . '   31 prts
' . '   32 fetch [0]
' . '   37 prti
' . '   38 push  1
' . '   43 prts
' . '   44 fetch [0]
' . '   49 push  1
' . '   54 add
' . '   55 store [0]
' . '   60 jmp    (-51) 10
' . '   65 halt
';
  $prog = parseProgram($programText);
  runVM($prog);
}
main();
