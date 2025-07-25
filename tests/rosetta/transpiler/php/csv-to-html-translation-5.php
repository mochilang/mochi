<?php
ini_set('memory_limit', '-1');
function split($s, $sep) {
  global $c, $rows, $cells;
  $out = [];
  $start = 0;
  $i = 0;
  $n = strlen($sep);
  while ($i <= strlen($s) - $n) {
  if (substr($s, $i, $i + $n - $i) == $sep) {
  $out = array_merge($out, [substr($s, $start, $i - $start)]);
  $i = $i + $n;
  $start = $i;
} else {
  $i = $i + 1;
}
};
  $out = array_merge($out, [substr($s, $start, strlen($s) - $start)]);
  return $out;
}
function htmlEscape($s) {
  global $c, $rows, $cells;
  $out = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == '&') {
  $out = $out . '&amp;';
} else {
  if ($ch == '<') {
  $out = $out . '&lt;';
} else {
  if ($ch == '>') {
  $out = $out . '&gt;';
} else {
  $out = $out . $ch;
};
};
}
  $i = $i + 1;
};
  return $out;
}
$c = 'Character,Speech
' . 'The multitude,The messiah! Show us the messiah!
' . 'Brians mother,<angry>Now you listen here! He\'s not the messiah; he\'s a very naughty boy! Now go away!</angry>
' . 'The multitude,Who are you?
' . 'Brians mother,I\'m his mother; that\'s who!
' . 'The multitude,Behold his mother! Behold his mother!';
$rows = [];
foreach (explode('
', $c) as $line) {
  $rows = array_merge($rows, [explode(',', $line)]);
}
echo rtrim('<table>'), PHP_EOL;
foreach ($rows as $row) {
  $cells = '';
  foreach ($row as $cell) {
  $cells = $cells . '<td>' . htmlEscape($cell) . '</td>';
};
  echo rtrim('    <tr>' . $cells . '</tr>'), PHP_EOL;
}
echo rtrim('</table>'), PHP_EOL;
