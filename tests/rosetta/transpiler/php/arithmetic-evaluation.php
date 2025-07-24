<?php
ini_set('memory_limit', '-1');
function skipWS($p) {
  global $parseIntStr, $parseNumber, $parseFactor, $powInt, $parsePower, $parseTerm, $parseExpr, $evalExpr, $main;
  $i = $p['pos'];
  while ($i < strlen($p['expr']) && substr($p['expr'], $i, $i + 1 - $i) == ' ') {
  $i = $i + 1;
};
  $p['pos'] = $i;
  return $p;
}
function parseIntStr($str) {
  global $skipWS, $parseNumber, $parseFactor, $powInt, $parsePower, $parseTerm, $parseExpr, $evalExpr, $main;
  $i = 0;
  $n = 0;
  while ($i < strlen($str)) {
  $n = $n * 10 + (ord(substr($str, $i, $i + 1 - $i))) - 48;
  $i = $i + 1;
};
  return $n;
}
function parseNumber($p) {
  global $skipWS, $parseIntStr, $parseFactor, $powInt, $parsePower, $parseTerm, $parseExpr, $evalExpr, $main;
  $p = skipWS($p);
  $start = $p['pos'];
  while ($p['pos'] < strlen($p['expr'])) {
  $ch = substr($p['expr'], $p['pos'], $p['pos'] + 1 - $p['pos']);
  if ($ch >= '0' && $ch <= '9') {
  $p['pos'] = $p['pos'] + 1;
} else {
  break;
}
};
  $token = substr($p['expr'], $start, $p['pos'] - $start);
  return ['v' => parseIntStr($token), 'p' => $p];
}
function parseFactor($p) {
  global $skipWS, $parseIntStr, $parseNumber, $powInt, $parsePower, $parseTerm, $parseExpr, $evalExpr, $main;
  $p = skipWS($p);
  if ($p['pos'] < strlen($p['expr']) && substr($p['expr'], $p['pos'], $p['pos'] + 1 - $p['pos']) == '(') {
  $p['pos'] = $p['pos'] + 1;
  $r = parseExpr($p);
  $v = $r['v'];
  $p = $r['p'];
  $p = skipWS($p);
  if ($p['pos'] < strlen($p['expr']) && substr($p['expr'], $p['pos'], $p['pos'] + 1 - $p['pos']) == ')') {
  $p['pos'] = $p['pos'] + 1;
};
  return ['v' => $v, 'p' => $p];
}
  if ($p['pos'] < strlen($p['expr']) && substr($p['expr'], $p['pos'], $p['pos'] + 1 - $p['pos']) == '-') {
  $p['pos'] = $p['pos'] + 1;
  $r = parseFactor($p);
  $v = $r['v'];
  $p = $r['p'];
  return ['v' => -$v, 'p' => $p];
}
  return parseNumber($p);
}
function powInt($base, $exp) {
  global $skipWS, $parseIntStr, $parseNumber, $parseFactor, $parsePower, $parseTerm, $parseExpr, $evalExpr, $main;
  $r = 1;
  $b = $base;
  $e = $exp;
  while ($e > 0) {
  if ($e % 2 == 1) {
  $r = $r * $b;
}
  $b = $b * $b;
  $e = $e / intval(2);
};
  return $r;
}
function parsePower($p) {
  global $skipWS, $parseIntStr, $parseNumber, $parseFactor, $powInt, $parseTerm, $parseExpr, $evalExpr, $main;
  $r = parseFactor($p);
  $v = $r['v'];
  $p = $r['p'];
  while (true) {
  $p = skipWS($p);
  if ($p['pos'] < strlen($p['expr']) && substr($p['expr'], $p['pos'], $p['pos'] + 1 - $p['pos']) == '^') {
  $p['pos'] = $p['pos'] + 1;
  $r2 = parseFactor($p);
  $rhs = $r2['v'];
  $p = $r2['p'];
  $v = powInt($v, $rhs);
} else {
  break;
}
};
  return ['v' => $v, 'p' => $p];
}
function parseTerm($p) {
  global $skipWS, $parseIntStr, $parseNumber, $parseFactor, $powInt, $parsePower, $parseExpr, $evalExpr, $main;
  $r = parsePower($p);
  $v = $r['v'];
  $p = $r['p'];
  while (true) {
  $p = skipWS($p);
  if ($p['pos'] < strlen($p['expr'])) {
  $op = substr($p['expr'], $p['pos'], $p['pos'] + 1 - $p['pos']);
  if ($op == '*') {
  $p['pos'] = $p['pos'] + 1;
  $r2 = parsePower($p);
  $rhs = $r2['v'];
  $p = $r2['p'];
  $v = $v * $rhs;
  continue;
};
  if ($op == '/') {
  $p['pos'] = $p['pos'] + 1;
  $r2 = parsePower($p);
  $rhs = $r2['v'];
  $p = $r2['p'];
  $v = $v / intval($rhs);
  continue;
};
}
  break;
};
  return ['v' => $v, 'p' => $p];
}
function parseExpr($p) {
  global $skipWS, $parseIntStr, $parseNumber, $parseFactor, $powInt, $parsePower, $parseTerm, $evalExpr, $main;
  $r = parseTerm($p);
  $v = $r['v'];
  $p = $r['p'];
  while (true) {
  $p = skipWS($p);
  if ($p['pos'] < strlen($p['expr'])) {
  $op = substr($p['expr'], $p['pos'], $p['pos'] + 1 - $p['pos']);
  if ($op == '+') {
  $p['pos'] = $p['pos'] + 1;
  $r2 = parseTerm($p);
  $rhs = $r2['v'];
  $p = $r2['p'];
  $v = $v + $rhs;
  continue;
};
  if ($op == '-') {
  $p['pos'] = $p['pos'] + 1;
  $r2 = parseTerm($p);
  $rhs = $r2['v'];
  $p = $r2['p'];
  $v = $v - $rhs;
  continue;
};
}
  break;
};
  return ['v' => $v, 'p' => $p];
}
function evalExpr($expr) {
  global $skipWS, $parseIntStr, $parseNumber, $parseFactor, $powInt, $parsePower, $parseTerm, $parseExpr, $main;
  $p = ['expr' => $expr, 'pos' => 0];
  $r = parseExpr($p);
  return $r['v'];
}
function main() {
  global $skipWS, $parseIntStr, $parseNumber, $parseFactor, $powInt, $parsePower, $parseTerm, $parseExpr, $evalExpr;
  $expr = '2*(3-1)+2*5';
  echo $expr . ' = ' . json_encode(evalExpr($expr), 1344), PHP_EOL;
}
main();
