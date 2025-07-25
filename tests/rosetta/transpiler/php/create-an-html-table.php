<?php
ini_set('memory_limit', '-1');
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
function main() {
  $rows = [];
  for ($i = 0; $i < 4; $i++) {
  $rows = array_merge($rows, [[$i * 3, $i * 3 + 1, $i * 3 + 2]]);
};
  echo rtrim('<table>'), PHP_EOL;
  echo rtrim('    <tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>'), PHP_EOL;
  $idx = 0;
  foreach ($rows as $row) {
  echo rtrim('    <tr><td>' . _str($idx) . '</td><td>' . _str($row[0]) . '</td><td>' . _str($row[1]) . '</td><td>' . _str($row[2]) . '</td></tr>'), PHP_EOL;
  $idx = $idx + 1;
};
  echo rtrim('</table>'), PHP_EOL;
}
main();
