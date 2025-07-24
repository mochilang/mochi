<?php
ini_set('memory_limit', '-1');
function square_to_maps($square) {
  global $remove_space, $encrypt, $decrypt, $main;
  $emap = [];
  $dmap = [];
  $x = 0;
  while ($x < count($square)) {
  $row = $square[$x];
  $y = 0;
  while ($y < count($row)) {
  $ch = $row[$y];
  $emap[$ch] = [$x, $y];
  $dmap[json_encode($x, 1344) . ',' . json_encode($y, 1344)] = $ch;
  $y = $y + 1;
};
  $x = $x + 1;
};
  return ['e' => $emap, 'd' => $dmap];
}
function remove_space($text, $emap) {
  global $square_to_maps, $encrypt, $decrypt, $main;
  $s = strtoupper($text);
  $out = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch != ' ' && array_key_exists($ch, $emap)) {
  $out = $out . $ch;
}
  $i = $i + 1;
};
  return $out;
}
function encrypt($text, $emap, $dmap) {
  global $square_to_maps, $remove_space, $decrypt, $main;
  $text = remove_space($text, $emap);
  $row0 = [];
  $row1 = [];
  $i = 0;
  while ($i < strlen($text)) {
  $ch = substr($text, $i, $i + 1 - $i);
  $xy = $emap[$ch];
  $row0 = array_merge($row0, [$xy[0]]);
  $row1 = array_merge($row1, [$xy[1]]);
  $i = $i + 1;
};
  foreach ($row1 as $v) {
  $row0 = array_merge($row0, [$v]);
};
  $res = '';
  $j = 0;
  while ($j < count($row0)) {
  $key = json_encode($row0[$j], 1344) . ',' . json_encode($row0[$j + 1], 1344);
  $res = $res . $dmap[$key];
  $j = $j + 2;
};
  return $res;
}
function decrypt($text, $emap, $dmap) {
  global $square_to_maps, $remove_space, $encrypt, $main;
  $text = remove_space($text, $emap);
  $coords = [];
  $i = 0;
  while ($i < strlen($text)) {
  $ch = substr($text, $i, $i + 1 - $i);
  $xy = $emap[$ch];
  $coords = array_merge($coords, [$xy[0]]);
  $coords = array_merge($coords, [$xy[1]]);
  $i = $i + 1;
};
  $half = count($coords) / 2;
  $k1 = [];
  $k2 = [];
  $idx = 0;
  while ($idx < $half) {
  $k1 = array_merge($k1, [$coords[$idx]]);
  $idx = $idx + 1;
};
  while ($idx < count($coords)) {
  $k2 = array_merge($k2, [$coords[$idx]]);
  $idx = $idx + 1;
};
  $res = '';
  $j = 0;
  while ($j < $half) {
  $key = json_encode($k1[$j], 1344) . ',' . json_encode($k2[$j], 1344);
  $res = $res . $dmap[$key];
  $j = $j + 1;
};
  return $res;
}
function main() {
  global $square_to_maps, $remove_space, $encrypt, $decrypt;
  $squareRosetta = [['A', 'B', 'C', 'D', 'E'], ['F', 'G', 'H', 'I', 'K'], ['L', 'M', 'N', 'O', 'P'], ['Q', 'R', 'S', 'T', 'U'], ['V', 'W', 'X', 'Y', 'Z'], ['J', '1', '2', '3', '4']];
  $squareWikipedia = [['B', 'G', 'W', 'K', 'Z'], ['Q', 'P', 'N', 'D', 'S'], ['I', 'O', 'A', 'X', 'E'], ['F', 'C', 'L', 'U', 'M'], ['T', 'H', 'Y', 'V', 'R'], ['J', '1', '2', '3', '4']];
  $textRosetta = '0ATTACKATDAWN';
  $textWikipedia = 'FLEEATONCE';
  $textTest = 'The invasion will start on the first of January';
  $maps = square_to_maps($squareRosetta);
  $emap = $maps['e'];
  $dmap = $maps['d'];
  echo rtrim('from Rosettacode'), PHP_EOL;
  echo rtrim('original:	 ' . $textRosetta), PHP_EOL;
  $s = encrypt($textRosetta, $emap, $dmap);
  echo rtrim('codiert:	 ' . $s), PHP_EOL;
  $s = decrypt($s, $emap, $dmap);
  echo rtrim('and back:	 ' . $s), PHP_EOL;
  $maps = square_to_maps($squareWikipedia);
  $emap = $maps['e'];
  $dmap = $maps['d'];
  echo rtrim('from Wikipedia'), PHP_EOL;
  echo rtrim('original:	 ' . $textWikipedia), PHP_EOL;
  $s = encrypt($textWikipedia, $emap, $dmap);
  echo rtrim('codiert:	 ' . $s), PHP_EOL;
  $s = decrypt($s, $emap, $dmap);
  echo rtrim('and back:	 ' . $s), PHP_EOL;
  $maps = square_to_maps($squareWikipedia);
  $emap = $maps['e'];
  $dmap = $maps['d'];
  echo rtrim('from Rosettacode long part'), PHP_EOL;
  echo rtrim('original:	 ' . $textTest), PHP_EOL;
  $s = encrypt($textTest, $emap, $dmap);
  echo rtrim('codiert:	 ' . $s), PHP_EOL;
  $s = decrypt($s, $emap, $dmap);
  echo rtrim('and back:	 ' . $s), PHP_EOL;
}
main();
