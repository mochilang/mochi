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
function parseIntStr($s, $base = 10) {
    return intval($s, intval($base));
}
$__start_mem = memory_get_usage();
$__start = _now();
  function fields($s) {
  $words = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == ' ' || $ch == '\n' || $ch == '\t') {
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
};
  function padRight($s, $width) {
  $out = $s;
  $i = strlen($s);
  while ($i < $width) {
  $out = $out . ' ';
  $i = $i + 1;
};
  return $out;
};
  function mochi_join($xs, $sep) {
  $res = '';
  $i = 0;
  while ($i < count($xs)) {
  if ($i > 0) {
  $res = $res . $sep;
}
  $res = $res . $xs[$i];
  $i = $i + 1;
};
  return $res;
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
  function isDigits($s) {
  if (strlen($s) == 0) {
  return false;
}
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch < '0' || $ch > '9') {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function readTable($table) {
  $toks = fields($table);
  $cmds = [];
  $mins = [];
  $i = 0;
  while ($i < count($toks)) {
  $cmd = $toks[$i];
  $minlen = strlen($cmd);
  $i = $i + 1;
  if ($i < count($toks) && isDigits($toks[$i])) {
  $num = parseIntStr($toks[$i], 10);
  if ($num >= 1 && $num < strlen($cmd)) {
  $minlen = $num;
  $i = $i + 1;
};
}
  $cmds = array_merge($cmds, [$cmd]);
  $mins = array_merge($mins, [$minlen]);
};
  return ['commands' => $cmds, 'mins' => $mins];
};
  function validate($commands, $mins, $words) {
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
  $results = array_merge($results, ['*error*']);
}
  $wi = $wi + 1;
};
  return $results;
};
  function main() {
  $table = '' . 'add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3 ' . 'compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate ' . '3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2 ' . 'forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load ' . 'locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2 ' . 'msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3 ' . 'refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left ' . '2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1 ';
  $sentence = 'riG   rePEAT copies  put mo   rest    types   fup.    6\npoweRin';
  $tbl = readTable($table);
  $commands = $tbl['commands'];
  $mins = $tbl['mins'];
  $words = fields($sentence);
  $results = validate($commands, $mins, $words);
  $out1 = 'user words:';
  $k = 0;
  while ($k < count($words)) {
  $out1 = $out1 . ' ';
  if ($k < count($words) - 1) {
  $out1 = $out1 . padRight($words[$k], strlen($results[$k]));
} else {
  $out1 = $out1 . $words[$k];
}
  $k = $k + 1;
};
  echo rtrim($out1), PHP_EOL;
  echo rtrim('full words: ' . mochi_join($results, ' ')), PHP_EOL;
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
