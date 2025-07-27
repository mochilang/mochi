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
$__start_mem = memory_get_usage();
$__start = _now();
  function split($s, $sep) {
  global $testSet;
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
  function trimSpace($s) {
  global $testSet;
  $start = 0;
  while ($start < strlen($s) && (substr($s, $start, $start + 1 - $start) == ' ' || substr($s, $start, $start + 1 - $start) == '\t')) {
  $start = $start + 1;
};
  $end = strlen($s);
  while ($end > $start && (substr($s, $end - 1, $end - ($end - 1)) == ' ' || substr($s, $end - 1, $end - ($end - 1)) == '\t')) {
  $end = $end - 1;
};
  return substr($s, $start, $end - $start);
};
  function indexOfSub($s, $sub) {
  global $testSet;
  if (strlen($sub) == 0) {
  return 0;
}
  $i = 0;
  while ($i + strlen($sub) <= strlen($s)) {
  if (substr($s, $i, $i + strlen($sub) - $i) == $sub) {
  return $i;
}
  $i = $i + 1;
};
  return 0 - 1;
};
  function parseRules($rs) {
  global $testSet;
  $rules = [];
  foreach (explode('
', $rs) as $line) {
  $ln = $line;
  $hash = indexOfSub($ln, '#');
  if ($hash >= 0) {
  $ln = substr($ln, 0, $hash - 0);
}
  $ln = trimSpace($ln);
  if (strlen($ln) == 0) {
  continue;
}
  $arrow = 0 - 1;
  $j = 0;
  while ($j + 2 <= strlen($ln)) {
  if (substr($ln, $j, $j + 2 - $j) == '->') {
  $pre = $j > 0 && (substr($ln, $j - 1, $j - ($j - 1)) == ' ' || substr($ln, $j - 1, $j - ($j - 1)) == '\t');
  $post = $j + 2 < strlen($ln) && (substr($ln, $j + 2, $j + 3 - ($j + 2)) == ' ' || substr($ln, $j + 2, $j + 3 - ($j + 2)) == '\t');
  if ($pre && $post) {
  $arrow = $j;
  break;
};
}
  $j = $j + 1;
};
  if ($arrow < 0) {
  $arrow = indexOfSub($ln, '->');
}
  if ($arrow < 0) {
  return ['ok' => false];
}
  $pat = trimSpace(substr($ln, 0, $arrow - 0));
  $rest = trimSpace(substr($ln, $arrow + 2, strlen($ln) - ($arrow + 2)));
  $term = false;
  if (strlen($rest) > 0 && substr($rest, 0, 1 - 0) == '.') {
  $term = true;
  $rest = substr($rest, 1, strlen($rest) - 1);
}
  $rep = $rest;
  $rules = array_merge($rules, [['pat' => $pat, 'rep' => $rep, 'term' => $term]]);
};
  return ['ok' => true, 'rules' => $rules];
};
  function runRules($rules, $s) {
  global $testSet;
  $changed = true;
  while ($changed) {
  $changed = false;
  $i = 0;
  while ($i < count($rules)) {
  $r = $rules[$i];
  $pat = $r['pat'];
  $rep = $r['rep'];
  $term = $r['term'];
  $idx = indexOfSub($s, $pat);
  if ($idx >= 0) {
  $s = substr($s, 0, $idx - 0) . $rep . substr($s, $idx + _len($pat));
  $changed = true;
  if ($term) {
  return $s;
};
  break;
}
  $i = $i + 1;
};
};
  return $s;
};
  function interpret($ruleset, $input) {
  global $testSet;
  $p = parseRules($ruleset);
  if (!$p['ok']) {
  return ['ok' => false, 'out' => ''];
}
  $out = runRules($p['rules'], $input);
  return ['ok' => true, 'out' => $out];
};
  $testSet = [['ruleSet' => '# This rules file is extracted from Wikipedia:
# http://en.wikipedia.org/wiki/Markov_Algorithm
A -> apple
B -> bag
S -> shop
T -> the
the shop -> my brother
a never used -> .terminating rule
', 'sample' => 'I bought a B of As from T S.', 'output' => 'I bought a bag of apples from my brother.'], ['ruleSet' => '# Slightly modified from the rules on Wikipedia
A -> apple
B -> bag
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule
', 'sample' => 'I bought a B of As from T S.', 'output' => 'I bought a bag of apples from T shop.'], ['ruleSet' => '# BNF Syntax testing rules
A -> apple
WWWW -> with
Bgage -> ->.*
B -> bag
->.* -> money
W -> WW
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule
', 'sample' => 'I bought a B of As W my Bgage from T S.', 'output' => 'I bought a bag of apples with my money from T shop.'], ['ruleSet' => '### Unary Multiplication Engine, for testing Markov Algorithm implementations
### By Donal Fellows.
# Unary addition engine
_+1 -> _1+
1+1 -> 11+
# Pass for converting from the splitting of multiplication into ordinary
# addition
1! -> !1
,! -> !+
_! -> _
# Unary multiplication by duplicating left side, right side times
1*1 -> x,@y
1x -> xX
X, -> 1,1
X1 -> 1X
_x -> _X
,x -> ,X
y1 -> 1y
y_ -> _
# Next phase of applying
1@1 -> x,@y
1@_ -> @_
,@_ -> !_
++ -> +
# Termination cleanup for addition
_1 -> 1
1+_ -> 1
_+_ ->
', 'sample' => '_1111*11111_', 'output' => '11111111111111111111'], ['ruleSet' => '# Turing machine: three-state busy beaver
#
# state A, symbol 0 => write 1, move right, new state B
A0 -> 1B
# state A, symbol 1 => write 1, move left, new state C
0A1 -> C01
1A1 -> C11
# state B, symbol 0 => write 1, move left, new state A
0B0 -> A01
1B0 -> A11
# state B, symbol 1 => write 1, move right, new state B
B1 -> 1B
# state C, symbol 0 => write 1, move left, new state B
0C0 -> B01
1C0 -> B11
# state C, symbol 1 => write 1, move left, halt
0C1 -> H01
1C1 -> H11
', 'sample' => '000000A000000', 'output' => '00011H1111000']];
  function main() {
  global $testSet;
  echo rtrim('validating ' . _str(count($testSet)) . ' test cases'), PHP_EOL;
  $failures = false;
  $i = 0;
  while ($i < count($testSet)) {
  $tc = $testSet[$i];
  $res = interpret($tc['ruleSet'], $tc['sample']);
  if (!$res['ok']) {
  echo rtrim('test ' . _str($i + 1) . ' invalid ruleset'), PHP_EOL;
  $failures = true;
} else {
  if ($res['out'] != $tc['output']) {
  echo rtrim('test ' . _str($i + 1) . ': got ' . $res['out'] . ', want ' . $tc['output']), PHP_EOL;
  $failures = true;
};
}
  $i = $i + 1;
};
  if (!$failures) {
  echo rtrim('no failures'), PHP_EOL;
}
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
