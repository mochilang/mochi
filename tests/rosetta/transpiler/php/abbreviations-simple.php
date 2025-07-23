<?php
function fields($s) {
  global $padRight, $mochi_join, $parseIntStr, $isDigits, $readTable, $validate, $main;
  $words = [];
  $cur = "";
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == " " || $ch == "\n" || $ch == "\t") {
  if (strlen($cur) > 0) {
  $words = array_merge($words, [$cur]);
  $cur = "";
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
function padRight($s, $width) {
  global $fields, $mochi_join, $parseIntStr, $isDigits, $readTable, $validate, $main;
  $out = $s;
  $i = strlen($s);
  while ($i < $width) {
  $out = $out . " ";
  $i = $i + 1;
};
  return $out;
}
function mochi_join($xs, $sep) {
  global $fields, $padRight, $parseIntStr, $isDigits, $readTable, $validate, $main;
  $res = "";
  $i = 0;
  while ($i < count($xs)) {
  if ($i > 0) {
  $res = $res . $sep;
}
  $res = $res . $xs[$i];
  $i = $i + 1;
};
  return $res;
}
function parseIntStr($str) {
  global $fields, $padRight, $mochi_join, $isDigits, $readTable, $validate, $main;
  $i = 0;
  $neg = false;
  if (strlen($str) > 0 && substr($str, 0, 1 - 0) == "-") {
  $neg = true;
  $i = 1;
}
  $n = 0;
  $digits = ["0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9];
  while ($i < strlen($str)) {
  $n = $n * 10 + $digits[substr($str, $i, $i + 1 - $i)];
  $i = $i + 1;
};
  if ($neg) {
  $n = -$n;
}
  return $n;
}
function isDigits($s) {
  global $fields, $padRight, $mochi_join, $parseIntStr, $readTable, $validate, $main;
  if (strlen($s) == 0) {
  return false;
}
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch < "0" || $ch > "9") {
  return false;
}
  $i = $i + 1;
};
  return true;
}
function readTable($table) {
  global $fields, $padRight, $mochi_join, $parseIntStr, $isDigits, $validate, $main;
  $toks = fields($table);
  $cmds = [];
  $mins = [];
  $i = 0;
  while ($i < count($toks)) {
  $cmd = $toks[$i];
  $minlen = strlen($cmd);
  $i = $i + 1;
  if ($i < count($toks) && isDigits($toks[$i])) {
  $num = parseIntStr($toks[$i]);
  if ($num >= 1 && $num < strlen($cmd)) {
  $minlen = $num;
  $i = $i + 1;
};
}
  $cmds = array_merge($cmds, [$cmd]);
  $mins = array_merge($mins, [$minlen]);
};
  return ["commands" => $cmds, "mins" => $mins];
}
function validate($commands, $mins, $words) {
  global $fields, $padRight, $mochi_join, $parseIntStr, $isDigits, $readTable, $main;
  $results = [];
  $wi = 0;
  while ($wi < count($words)) {
  $w = $words[$wi];
  $found = false;
  $wlen = strlen($w);
  $ci = 0;
  while ($ci < count($commands)) {
  $cmd = $commands[$ci];
  if ($mins[$ci] != 0 && $wlen >= $mins[$ci] && $wlen <= strlen($cmd)) {
  $c = strtoupper($cmd);
  $ww = strtoupper($w);
  if (substr($c, 0, $wlen - 0) == $ww) {
  $results = array_merge($results, [$c]);
  $found = true;
  break;
};
}
  $ci = $ci + 1;
};
  if (!$found) {
  $results = array_merge($results, ["*error*"]);
}
  $wi = $wi + 1;
};
  return $results;
}
function main() {
  global $fields, $padRight, $mochi_join, $parseIntStr, $isDigits, $readTable, $validate;
  $table = "" . "add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3 " . "compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate " . "3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2 " . "forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load " . "locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2 " . "msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3 " . "refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left " . "2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1 ";
  $sentence = "riG   rePEAT copies  put mo   rest    types   fup.    6\npoweRin";
  $tbl = readTable($table);
  $commands = $tbl["commands"];
  $mins = $tbl["mins"];
  $words = fields($sentence);
  $results = validate($commands, $mins, $words);
  $out1 = "user words:";
  $k = 0;
  while ($k < count($words)) {
  $out1 = $out1 . " ";
  if ($k < count($words) - 1) {
  $out1 = $out1 . padRight($words[$k], strlen($results[$k]));
} else {
  $out1 = $out1 . $words[$k];
}
  $k = $k + 1;
};
  echo $out1, PHP_EOL;
  echo "full words: " . mochi_join($results, " "), PHP_EOL;
}
main();
