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
  function validate($commands, $words, $mins) {
  $results = [];
  if (count($words) == 0) {
  return $results;
}
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
  $table = 'Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress Copy ' . 'COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find ' . 'NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput ' . ' Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO ' . 'MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT ' . 'READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT ' . 'RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer TypeUp ';
  $commands = fields($table);
  $mins = [];
  $i = 0;
  while ($i < count($commands)) {
  $count = 0;
  $j = 0;
  $cmd = $commands[$i];
  while ($j < strlen($cmd)) {
  $ch = substr($cmd, $j, $j + 1 - $j);
  if ($ch >= 'A' && $ch <= 'Z') {
  $count = $count + 1;
}
  $j = $j + 1;
};
  $mins = array_merge($mins, [$count]);
  $i = $i + 1;
};
  $sentence = 'riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin';
  $words = fields($sentence);
  $results = validate($commands, $words, $mins);
  $out1 = 'user words:  ';
  $k = 0;
  while ($k < count($words)) {
  $out1 = $out1 . padRight($words[$k], strlen($results[$k])) . ' ';
  $k = $k + 1;
};
  echo rtrim($out1), PHP_EOL;
  echo rtrim('full words:  ' . mochi_join($results, ' ')), PHP_EOL;
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
