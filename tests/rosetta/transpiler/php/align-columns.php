<?php
function split($s, $sep) {
  global $rstripEmpty, $spaces, $pad, $newFormatter, $printFmt, $text, $f;
  $parts = [];
  $cur = "";
  $i = 0;
  while ($i < strlen($s)) {
  if (strlen($sep) > 0 && $i + strlen($sep) <= strlen($s) && substr($s, $i, $i + strlen($sep) - $i) == $sep) {
  $parts = array_merge($parts, [$cur]);
  $cur = "";
  $i = $i + strlen($sep);
} else {
  $cur = $cur . substr($s, $i, $i + 1 - $i);
  $i = $i + 1;
}
};
  $parts = array_merge($parts, [$cur]);
  return $parts;
}
function rstripEmpty(&$words) {
  global $split, $spaces, $pad, $newFormatter, $printFmt, $text, $f;
  $n = count($words);
  while ($n > 0 && $words[$n - 1] == "") {
  $n = $n - 1;
};
  return array_slice($words, 0, $n - 0);
}
function spaces($n) {
  global $split, $rstripEmpty, $pad, $newFormatter, $printFmt, $text, $f;
  $out = "";
  $i = 0;
  while ($i < $n) {
  $out = $out . " ";
  $i = $i + 1;
};
  return $out;
}
function pad($word, $width, $align) {
  global $split, $rstripEmpty, $spaces, $newFormatter, $printFmt, $text, $f;
  $diff = $width - strlen($word);
  if ($align == 0) {
  return $word . spaces($diff);
}
  if ($align == 2) {
  return spaces($diff) . $word;
}
  $left = intval((intdiv($diff, 2)));
  $right = $diff - $left;
  return spaces($left) . $word . spaces($right);
}
function newFormatter($text) {
  global $split, $rstripEmpty, $spaces, $pad, $printFmt, $f;
  $lines = split($text, "\n");
  $fmtLines = [];
  $width = [];
  $i = 0;
  while ($i < count($lines)) {
  if (strlen($lines[$i]) == 0) {
  $i = $i + 1;
  continue;
}
  $words = rstripEmpty(split($lines[$i], "$"));
  $fmtLines = array_merge($fmtLines, [$words]);
  $j = 0;
  while ($j < count($words)) {
  $wlen = strlen($words[$j]);
  if ($j == count($width)) {
  $width = array_merge($width, [$wlen]);
} else {
  if ($wlen > $width[$j]) {
  $width[$j] = $wlen;
};
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return ["text" => $fmtLines, "width" => $width];
}
function printFmt(&$f, $align) {
  global $split, $rstripEmpty, $spaces, $pad, $newFormatter, $text;
  $lines = $f["text"];
  $width = $f["width"];
  $i = 0;
  while ($i < count($lines)) {
  $words = $lines[$i];
  $line = "";
  $j = 0;
  while ($j < count($words)) {
  $line = $line . pad($words[$j], $width[$j], $align) . " ";
  $j = $j + 1;
};
  echo $line, PHP_EOL;
  $i = $i + 1;
};
  echo "", PHP_EOL;
}
$text = "Given$a$text$file$of$many$lines,$where$fields$within$a$line\n" . "are$delineated$by$a$single$'dollar'$character,$write$a$program\n" . "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each\n" . "column$are$separated$by$at$least$one$space.\n" . "Further,$allow$for$each$word$in$a$column$to$be$either$left\n" . "justified,$right$justified,$or$center$justified$within$its$column.";
$f = newFormatter($text);
printFmt($f, 0);
printFmt($f, 1);
printFmt($f, 2);
