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
function parseIntStr($s, $base = 10) {
    return intval($s, intval($base));
}
$__start_mem = memory_get_usage();
$__start = _now();
  function mochi_trim($s) {
  $start = 0;
  while ($start < strlen($s) && (substr($s, $start, $start + 1 - $start) == ' ' || substr($s, $start, $start + 1 - $start) == '	')) {
  $start = $start + 1;
};
  $end = strlen($s);
  while ($end > $start && (substr($s, $end - 1, $end - ($end - 1)) == ' ' || substr($s, $end - 1, $end - ($end - 1)) == '	')) {
  $end = $end - 1;
};
  return substr($s, $start, $end - $start);
};
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
};
  function splitWS($s) {
  $out = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == ' ' || $ch == '	') {
  if (strlen($cur) > 0) {
  $out = array_merge($out, [$cur]);
  $cur = '';
};
} else {
  $cur = $cur . $ch;
}
  $i = $i + 1;
};
  if (strlen($cur) > 0) {
  $out = array_merge($out, [$cur]);
}
  return $out;
};
  function indexOf($s, $ch) {
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
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
};
  function parseAsm($asm) {
  $lines = explode('
', $asm);
  $instrs = [];
  $labels = [];
  $lineNum = 0;
  $i = 0;
  while ($i < count($lines)) {
  $line = $lines[$i];
  if (_indexof($line, ';') != (-1)) {
  $line = substr($line, 0, _indexof($line, ';') - 0);
}
  $line = mochi_trim($line);
  $label = '';
  if (_indexof($line, ':') != (-1)) {
  $idx = _indexof($line, ':');
  $label = mochi_trim(substr($line, 0, $idx - 0));
  $line = mochi_trim(substr($line, $idx + 1, strlen($line) - ($idx + 1)));
}
  $opcode = '';
  $arg = '';
  if (strlen($line) > 0) {
  $parts = splitWS($line);
  if (count($parts) > 0) {
  $opcode = $parts[0];
};
  if (count($parts) > 1) {
  $arg = $parts[1];
} else {
  $ops = ['NOP' => 0, 'LDA' => 1, 'STA' => 2, 'ADD' => 3, 'SUB' => 4, 'BRZ' => 5, 'JMP' => 6, 'STP' => 7];
  if (!(isset($ops[$opcode]))) {
  $arg = $opcode;
  $opcode = '';
};
};
}
  if ($label != '') {
  $labels[$label] = $lineNum;
}
  $instrs = array_merge($instrs, [['Label' => $label, 'Opcode' => $opcode, 'Arg' => $arg]]);
  $lineNum = $lineNum + 1;
  $i = $i + 1;
};
  return ['instructions' => $instrs, 'labels' => $labels];
};
  function compile($p) {
  $instrs = $p['instructions'];
  $labels = $p['labels'];
  $bytecode = [];
  $i = 0;
  $opcodes = ['NOP' => 0, 'LDA' => 1, 'STA' => 2, 'ADD' => 3, 'SUB' => 4, 'BRZ' => 5, 'JMP' => 6, 'STP' => 7];
  while ($i < count($instrs)) {
  $ins = $instrs[$i];
  $arg = 0;
  if ($ins['Arg'] != '') {
  if (array_key_exists($ins['Arg'], $labels)) {
  $arg = $labels[$ins['Arg']];
} else {
  $arg = parseIntStr($ins['Arg'], 10);
};
}
  $code = 0;
  if ($ins['Opcode'] != '') {
  $code = $opcodes[$ins['Opcode']];
}
  $bytecode = array_merge($bytecode, [$code * 32 + $arg]);
  $i = $i + 1;
};
  while (count($bytecode) < 32) {
  $bytecode = array_merge($bytecode, [0]);
};
  return $bytecode;
};
  function floorMod($a, $b) {
  $r = $a % $b;
  if ($r < 0) {
  $r = $r + $b;
}
  return $r;
};
  function run($bytecode) {
  $acc = 0;
  $pc = 0;
  $mem = [];
  $i = 0;
  while ($i < count($bytecode)) {
  $mem = array_merge($mem, [$bytecode[$i]]);
  $i = $i + 1;
};
  while ($pc < 32) {
  $op = $mem[$pc] / 32;
  $arg = fmod($mem[$pc], 32);
  $pc = $pc + 1;
  if ($op == 0) {
  continue;
} else {
  if ($op == 1) {
  $acc = $mem[$arg];
} else {
  if ($op == 2) {
  $mem[$arg] = $acc;
} else {
  if ($op == 3) {
  $acc = floorMod($acc + $mem[$arg], 256);
} else {
  if ($op == 4) {
  $acc = floorMod($acc - $mem[$arg], 256);
} else {
  if ($op == 5) {
  if ($acc == 0) {
  $pc = $arg;
};
} else {
  if ($op == 6) {
  $pc = $arg;
} else {
  if ($op == 7) {
  break;
} else {
  break;
};
};
};
};
};
};
};
}
};
  return $acc;
};
  function execute($asm) {
  $parsed = parseAsm($asm);
  $bc = compile($parsed);
  return run($bc);
};
  function main() {
  $examples = ['LDA   x
' . 'ADD   y       ; accumulator = x + y
' . 'STP
' . 'x:            2
' . 'y:            2', 'loop:   LDA   prodt
' . '        ADD   x
' . '        STA   prodt
' . '        LDA   y
' . '        SUB   one
' . '        STA   y
' . '        BRZ   done
' . '        JMP   loop
' . 'done:   LDA   prodt   ; to display it
' . '        STP
' . 'x:            8
' . 'y:            7
' . 'prodt:        0
' . 'one:          1', 'loop:   LDA   n
' . '        STA   temp
' . '        ADD   m
' . '        STA   n
' . '        LDA   temp
' . '        STA   m
' . '        LDA   count
' . '        SUB   one
' . '        BRZ   done
' . '        STA   count
' . '        JMP   loop
' . 'done:   LDA   n       ; to display it
' . '        STP
' . 'm:            1
' . 'n:            1
' . 'temp:         0
' . 'count:        8       ; valid range: 1-11
' . 'one:          1', 'start:  LDA   load
' . 'ADD   car     ; head of list
' . 'STA   ldcar
' . 'ADD   one
' . 'STA   ldcdr   ; next CONS cell
' . 'ldcar:  NOP
' . 'STA   value
' . 'ldcdr:  NOP
' . 'BRZ   done    ; 0 stands for NIL
' . 'STA   car
' . 'JMP   start
' . 'done:   LDA   value   ; CAR of last CONS
' . 'STP
' . 'load:   LDA   0
' . 'value:        0
' . 'car:          28
' . 'one:          1
' . '                        ; order of CONS cells
' . '                        ; in memory
' . '                        ; does not matter
' . '        6
' . '        0       ; 0 stands for NIL
' . '        2       ; (CADR ls)
' . '        26      ; (CDDR ls) -- etc.
' . '        5
' . '        20
' . '        3
' . '        30
' . '        1       ; value of (CAR ls)
' . '        22      ; points to (CDR ls)
' . '        4
' . '        24', 'LDA  3
' . 'SUB  4
' . 'STP  0
' . '         0
' . '         255', 'LDA  3
' . 'SUB  4
' . 'STP  0
' . '                0
' . '                1', 'LDA  3
' . 'ADD  4
' . 'STP  0
' . '                1
' . '                255'];
  $i = 0;
  while ($i < count($examples)) {
  $res = execute($examples[$i]);
  echo rtrim(_str($res)), PHP_EOL;
  $i = $i + 1;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
