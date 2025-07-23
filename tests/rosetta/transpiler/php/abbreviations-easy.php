<?php
ini_set('memory_limit','-1');
function _append($a, $b) {
    $a[] = $b;
    return $a;
}
function fields($s) {
  global $padRight, $mochi_join, $validate, $main;
  $words = [];
  $cur = "";
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == " " || $ch == "\n" || $ch == "\t") {
  if (strlen($cur) > 0) {
  $words = _append($words, $cur);
  $cur = "";
};
} else {
  $cur = $cur . $ch;
}
  $i = $i + 1;
};
  if (strlen($cur) > 0) {
  $words = _append($words, $cur);
}
  return $words;
}
function padRight($s, $width) {
  global $fields, $mochi_join, $validate, $main;
  $out = $s;
  $i = strlen($s);
  while ($i < $width) {
  $out = $out . " ";
  $i = $i + 1;
};
  return $out;
}
function mochi_join($xs, $sep) {
  global $fields, $padRight, $validate, $main;
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
function validate($commands, $words, $mins) {
  global $fields, $padRight, $mochi_join, $main;
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
  $results = _append($results, $c);
  $found = true;
  break;
};
}
  $ci = $ci + 1;
};
  if (!$found) {
  $results = _append($results, "*error*");
}
  $wi = $wi + 1;
};
  return $results;
}
function main() {
  global $fields, $padRight, $mochi_join, $validate;
  $table = "Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress Copy " . "COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find " . "NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput " . " Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO " . "MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT " . "READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT " . "RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer TypeUp ";
  $commands = fields($table);
  $mins = [];
  $i = 0;
  while ($i < count($commands)) {
  $count = 0;
  $j = 0;
  $cmd = $commands[$i];
  while ($j < strlen($cmd)) {
  $ch = substr($cmd, $j, $j + 1 - $j);
  if ($ch >= "A" && $ch <= "Z") {
  $count = $count + 1;
}
  $j = $j + 1;
};
  $mins = _append($mins, $count);
  $i = $i + 1;
};
  $sentence = "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin";
  $words = fields($sentence);
  $results = validate($commands, $words, $mins);
  $out1 = "user words:  ";
  $k = 0;
  while ($k < count($words)) {
  $out1 = $out1 . padRight($words[$k], strlen($results[$k])) . " ";
  $k = $k + 1;
};
  echo $out1, PHP_EOL;
  echo "full words:  " . mochi_join($results, " "), PHP_EOL;
}
main();
