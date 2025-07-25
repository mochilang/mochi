<?php
ini_set('memory_limit', '-1');
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
  $cells = $cells . '<td>' . $cell . '</td>';
};
  echo rtrim('    <tr>' . $cells . '</tr>'), PHP_EOL;
}
echo rtrim('</table>'), PHP_EOL;
