<?php
ini_set('memory_limit', '-1');
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
function indexOf($s, $ch) {
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
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
function makePatterns() {
  $digits = ['1', '2', '3', '4', '5', '6', '7', '8', '9'];
  $pats = [];
  $i = 0;
  while ($i < count($digits)) {
  $j = 0;
  while ($j < count($digits)) {
  if ($j != $i) {
  $k = 0;
  while ($k < count($digits)) {
  if ($k != $i && $k != $j) {
  $l = 0;
  while ($l < count($digits)) {
  if ($l != $i && $l != $j && $l != $k) {
  $pats = array_merge($pats, [$digits[$i] . $digits[$j] . $digits[$k] . $digits[$l]]);
}
  $l = $l + 1;
};
}
  $k = $k + 1;
};
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $pats;
}
function main() {
  echo rtrim('Cows and bulls/player
' . 'You think of four digit number of unique digits in the range 1 to 9.
' . 'I guess.  You score my guess:
' . '    A correct digit but not in the correct place is a cow.
' . '    A correct digit in the correct place is a bull.
' . 'You give my score as two numbers separated with a space.'), PHP_EOL;
  $patterns = makePatterns();
  while (true) {
  if (count($patterns) == 0) {
  echo rtrim('Oops, check scoring.'), PHP_EOL;
  return;
}
  $guess = $patterns[0];
  $patterns = array_slice($patterns, 1);
  $cows = 0;
  $bulls = 0;
  while (true) {
  echo rtrim('My guess: ' . $guess . '.  Score? (c b) '), PHP_EOL;
  $line = trim(fgets(STDIN));
  $toks = fields($line);
  if (count($toks) == 2) {
  $c = intval($toks[0]);
  $b = intval($toks[1]);
  if ($c >= 0 && $c <= 4 && $b >= 0 && $b <= 4 && $c + $b <= 4) {
  $cows = $c;
  $bulls = $b;
  break;
};
}
  echo rtrim('Score guess as two numbers: cows bulls'), PHP_EOL;
};
  if ($bulls == 4) {
  echo rtrim('I did it. :)'), PHP_EOL;
  return;
}
  $next = [];
  $idx = 0;
  while ($idx < count($patterns)) {
  $pat = $patterns[$idx];
  $c = 0;
  $b = 0;
  $i = 0;
  while ($i < 4) {
  $cg = substr($guess, $i, $i + 1 - $i);
  $cp = substr($pat, $i, $i + 1 - $i);
  if ($cg == $cp) {
  $b = $b + 1;
} else {
  if (_indexof($pat, $cg) >= 0) {
  $c = $c + 1;
};
}
  $i = $i + 1;
};
  if ($c == $cows && $b == $bulls) {
  $next = array_merge($next, [$pat]);
}
  $idx = $idx + 1;
};
  $patterns = $next;
};
}
main();
