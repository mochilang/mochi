<?php
ini_set('memory_limit', '-1');
function randInt($s, $n) {
  global $padLeft, $makeSeq, $mutate, $prettyPrint, $wstring, $main;
  $next = ($s * 1664525 + 1013904223) % 2147483647;
  return [$next, $next % $n];
}
function padLeft($s, $w) {
  global $randInt, $makeSeq, $mutate, $prettyPrint, $wstring, $main;
  $res = '';
  $n = $w - strlen($s);
  while ($n > 0) {
  $res = $res . ' ';
  $n = $n - 1;
};
  return $res . $s;
}
function makeSeq($s, $le) {
  global $randInt, $padLeft, $mutate, $prettyPrint, $wstring, $main;
  $bases = 'ACGT';
  $out = '';
  $i = 0;
  while ($i < $le) {
  $r = randInt($s, 4);
  $s = $r[0];
  $idx = intval($r[1]);
  $out = $out . substr($bases, $idx, $idx + 1 - $idx);
  $i = $i + 1;
};
  return [$s, $out];
}
function mutate($s, $dna, $w) {
  global $randInt, $padLeft, $makeSeq, $prettyPrint, $wstring, $main;
  $bases = 'ACGT';
  $le = strlen($dna);
  $r = randInt($s, $le);
  $s = $r[0];
  $p = intval($r[1]);
  $r = randInt($s, 300);
  $s = $r[0];
  $x = intval($r[1]);
  $arr = [];
  $i = 0;
  while ($i < $le) {
  $arr = array_merge($arr, [substr($dna, $i, $i + 1 - $i)]);
  $i = $i + 1;
};
  if ($x < $w[0]) {
  $r = randInt($s, 4);
  $s = $r[0];
  $idx = intval($r[1]);
  $b = substr($bases, $idx, $idx + 1 - $idx);
  echo rtrim('  Change @' . padLeft(json_encode($p, 1344), 3) . ' \'' . $arr[$p] . '\' to \'' . $b . '\''), PHP_EOL;
  $arr[$p] = $b;
} else {
  if ($x < $w[0] + $w[1]) {
  echo rtrim('  Delete @' . padLeft(json_encode($p, 1344), 3) . ' \'' . $arr[$p] . '\''), PHP_EOL;
  $j = $p;
  while ($j < count($arr) - 1) {
  $arr[$j] = $arr[$j + 1];
  $j = $j + 1;
};
  $arr = array_slice($arr, 0, count($arr) - 1 - 0);
} else {
  $r = randInt($s, 4);
  $s = $r[0];
  $idx2 = intval($r[1]);
  $b = substr($bases, $idx2, $idx2 + 1 - $idx2);
  $arr = array_merge($arr, ['']);
  $j = count($arr) - 1;
  while ($j > $p) {
  $arr[$j] = $arr[$j - 1];
  $j = $j - 1;
};
  echo rtrim('  Insert @' . padLeft(json_encode($p, 1344), 3) . ' \'' . $b . '\''), PHP_EOL;
  $arr[$p] = $b;
};
}
  $out = '';
  $i = 0;
  while ($i < count($arr)) {
  $out = $out . $arr[$i];
  $i = $i + 1;
};
  return [$s, $out];
}
function prettyPrint($dna, $rowLen) {
  global $randInt, $padLeft, $makeSeq, $mutate, $wstring, $main;
  echo rtrim('SEQUENCE:'), PHP_EOL;
  $le = strlen($dna);
  $i = 0;
  while ($i < $le) {
  $k = $i + $rowLen;
  if ($k > $le) {
  $k = $le;
}
  echo rtrim(padLeft(json_encode($i, 1344), 5) . ': ' . substr($dna, $i, $k - $i)), PHP_EOL;
  $i = $i + $rowLen;
};
  $a = 0;
  $c = 0;
  $g = 0;
  $t = 0;
  $idx = 0;
  while ($idx < $le) {
  $ch = substr($dna, $idx, $idx + 1 - $idx);
  if ($ch == 'A') {
  $a = $a + 1;
} else {
  if ($ch == 'C') {
  $c = $c + 1;
} else {
  if ($ch == 'G') {
  $g = $g + 1;
} else {
  if ($ch == 'T') {
  $t = $t + 1;
};
};
};
}
  $idx = $idx + 1;
};
  echo rtrim(''), PHP_EOL;
  echo rtrim('BASE COUNT:'), PHP_EOL;
  echo rtrim('    A: ' . padLeft(json_encode($a, 1344), 3)), PHP_EOL;
  echo rtrim('    C: ' . padLeft(json_encode($c, 1344), 3)), PHP_EOL;
  echo rtrim('    G: ' . padLeft(json_encode($g, 1344), 3)), PHP_EOL;
  echo rtrim('    T: ' . padLeft(json_encode($t, 1344), 3)), PHP_EOL;
  echo rtrim('    ------'), PHP_EOL;
  echo rtrim('    Σ: ' . json_encode($le, 1344)), PHP_EOL;
  echo rtrim('    ======'), PHP_EOL;
}
function wstring($w) {
  global $randInt, $padLeft, $makeSeq, $mutate, $prettyPrint, $main;
  return '  Change: ' . json_encode($w[0], 1344) . '
  Delete: ' . json_encode($w[1], 1344) . '
  Insert: ' . json_encode($w[2], 1344) . '
';
}
function main() {
  global $randInt, $padLeft, $makeSeq, $mutate, $prettyPrint, $wstring;
  $seed = 1;
  $res = makeSeq($seed, 250);
  $seed = $res[0];
  $dna = strval($res[1]);
  prettyPrint($dna, 50);
  $muts = 10;
  $w = [100, 100, 100];
  echo rtrim('
WEIGHTS (ex 300):'), PHP_EOL;
  echo rtrim(wstring($w)), PHP_EOL;
  echo rtrim('MUTATIONS (' . json_encode($muts, 1344) . '):'), PHP_EOL;
  $i = 0;
  while ($i < $muts) {
  $res = mutate($seed, $dna, $w);
  $seed = $res[0];
  $dna = strval($res[1]);
  $i = $i + 1;
};
  echo rtrim(''), PHP_EOL;
  prettyPrint($dna, 50);
}
main();
