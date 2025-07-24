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
function primesUpTo($n) {
  global $LIMIT, $primes;
  $sieve = [];
  $i = 0;
  while ($i <= $n) {
  $sieve = array_merge($sieve, [true]);
  $i = $i + 1;
};
  $p = 2;
  while ($p * $p <= $n) {
  if ($sieve[$p]) {
  $m = $p * $p;
  while ($m <= $n) {
  $sieve[$m] = false;
  $m = $m + $p;
};
}
  $p = $p + 1;
};
  $res = [];
  $x = 2;
  while ($x <= $n) {
  if ($sieve[$x]) {
  $res = array_merge($res, [$x]);
}
  $x = $x + 1;
};
  return $res;
}
$LIMIT = 999999;
$primes = primesUpTo($LIMIT);
function longestSeq($dir) {
  global $LIMIT, $primes;
  $pd = 0;
  $longSeqs = [[2]];
  $currSeq = [2];
  $i = 1;
  while ($i < count($primes)) {
  $d = $primes[$i] - $primes[$i - 1];
  if (($dir == 'ascending' && $d <= $pd) || ($dir == 'descending' && $d >= $pd)) {
  if (count($currSeq) > count($longSeqs[0])) {
  $longSeqs = [$currSeq];
} else {
  if (count($currSeq) == count($longSeqs[0])) {
  $longSeqs = array_merge($longSeqs, [$currSeq]);
};
};
  $currSeq = [$primes[$i - 1], $primes[$i]];
} else {
  $currSeq = array_merge($currSeq, [$primes[$i]]);
}
  $pd = $d;
  $i = $i + 1;
};
  if (count($currSeq) > count($longSeqs[0])) {
  $longSeqs = [$currSeq];
} else {
  if (count($currSeq) == count($longSeqs[0])) {
  $longSeqs = array_merge($longSeqs, [$currSeq]);
};
}
  echo rtrim('Longest run(s) of primes with ' . $dir . ' differences is ' . _str(count($longSeqs[0])) . ' :'), PHP_EOL;
  foreach ($longSeqs as $ls) {
  $diffs = [];
  $j = 1;
  while ($j < count($ls)) {
  $diffs = array_merge($diffs, [$ls[$j] - $ls[$j - 1]]);
  $j = $j + 1;
};
  $k = 0;
  while ($k < count($ls) - 1) {
  echo rtrim(_str($ls[$k]) . ' (' . _str($diffs[$k]) . ') ') . " " . rtrim((false ? 'true' : 'false')), PHP_EOL;
  $k = $k + 1;
};
  echo rtrim(json_encode(_str($ls[count($ls) - 1]), 1344)), PHP_EOL;
};
  echo rtrim(''), PHP_EOL;
}
function main() {
  global $LIMIT, $primes;
  echo rtrim('For primes < 1 million:
'), PHP_EOL;
  foreach (['ascending', 'descending'] as $dir) {
  longestSeq($dir);
};
}
main();
