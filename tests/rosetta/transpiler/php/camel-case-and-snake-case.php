<?php
ini_set('memory_limit', '-1');
function trimSpace($s) {
  global $isUpper, $padLeft, $snakeToCamel, $camelToSnake, $main;
  $start = 0;
  while ($start < strlen($s) && substr($s, $start, $start + 1 - $start) == ' ') {
  $start = $start + 1;
};
  $end = strlen($s);
  while ($end > $start && substr($s, $end - 1, $end - $end - 1) == ' ') {
  $end = $end - 1;
};
  return substr($s, $start, $end - $start);
}
function isUpper($ch) {
  global $trimSpace, $padLeft, $snakeToCamel, $camelToSnake, $main;
  return $ch >= 'A' && $ch <= 'Z';
}
function padLeft($s, $w) {
  global $trimSpace, $isUpper, $snakeToCamel, $camelToSnake, $main;
  $res = '';
  $n = $w - strlen($s);
  while ($n > 0) {
  $res = $res . ' ';
  $n = $n - 1;
};
  return $res . $s;
}
function snakeToCamel($s) {
  global $trimSpace, $isUpper, $padLeft, $camelToSnake, $main;
  $s = trimSpace($s);
  $out = '';
  $up = false;
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == '_' || $ch == '-' || $ch == ' ' || $ch == '.') {
  $up = true;
  $i = $i + 1;
  continue;
}
  if ($i == 0) {
  $out = $out . strtolower($ch);
  $up = false;
  $i = $i + 1;
  continue;
}
  if ($up) {
  $out = $out . strtoupper($ch);
  $up = false;
} else {
  $out = $out . $ch;
}
  $i = $i + 1;
};
  return $out;
}
function camelToSnake($s) {
  global $trimSpace, $isUpper, $padLeft, $snakeToCamel, $main;
  $s = trimSpace($s);
  $out = '';
  $prevUnd = false;
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == ' ' || $ch == '-' || $ch == '.') {
  if (!$prevUnd && strlen($out) > 0) {
  $out = $out . '_';
  $prevUnd = true;
};
  $i = $i + 1;
  continue;
}
  if ($ch == '_') {
  if (!$prevUnd && strlen($out) > 0) {
  $out = $out . '_';
  $prevUnd = true;
};
  $i = $i + 1;
  continue;
}
  if (isUpper($ch)) {
  if ($i > 0 && (!$prevUnd)) {
  $out = $out . '_';
};
  $out = $out . strtolower($ch);
  $prevUnd = false;
} else {
  $out = $out . strtolower($ch);
  $prevUnd = false;
}
  $i = $i + 1;
};
  $start = 0;
  while ($start < strlen($out) && substr($out, $start, $start + 1 - $start) == '_') {
  $start = $start + 1;
};
  $end = strlen($out);
  while ($end > $start && substr($out, $end - 1, $end - $end - 1) == '_') {
  $end = $end - 1;
};
  $out = substr($out, $start, $end - $start);
  $res = '';
  $j = 0;
  $lastUnd = false;
  while ($j < strlen($out)) {
  $c = substr($out, $j, $j + 1 - $j);
  if ($c == '_') {
  if (!$lastUnd) {
  $res = $res . $c;
};
  $lastUnd = true;
} else {
  $res = $res . $c;
  $lastUnd = false;
}
  $j = $j + 1;
};
  return $res;
}
function main() {
  global $trimSpace, $isUpper, $padLeft, $snakeToCamel, $camelToSnake;
  $samples = ['snakeCase', 'snake_case', 'snake-case', 'snake case', 'snake CASE', 'snake.case', 'variable_10_case', 'variable10Case', 'ɛrgo rE tHis', 'hurry-up-joe!', 'c://my-docs/happy_Flag-Day/12.doc', ' spaces '];
  echo rtrim('=== To snake_case ==='), PHP_EOL;
  foreach ($samples as $s) {
  echo rtrim(padLeft($s, 34) . ' => ' . camelToSnake($s)), PHP_EOL;
};
  echo rtrim(''), PHP_EOL;
  echo rtrim('=== To camelCase ==='), PHP_EOL;
  foreach ($samples as $s) {
  echo rtrim(padLeft($s, 34) . ' => ' . snakeToCamel($s)), PHP_EOL;
};
}
main();
