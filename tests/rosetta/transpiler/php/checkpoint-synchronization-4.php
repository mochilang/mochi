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
$nMech = 5;
$detailsPerMech = 4;
for ($mech = 1; $mech < ($nMech + 1); $mech++) {
  $id = $mech;
  echo rtrim('worker ' . _str($id) . ' contracted to assemble ' . _str($detailsPerMech) . ' details'), PHP_EOL;
  echo rtrim('worker ' . _str($id) . ' enters shop'), PHP_EOL;
  $d = 0;
  while ($d < $detailsPerMech) {
  echo rtrim('worker ' . _str($id) . ' assembling'), PHP_EOL;
  echo rtrim('worker ' . _str($id) . ' completed detail'), PHP_EOL;
  $d = $d + 1;
};
  echo rtrim('worker ' . _str($id) . ' leaves shop'), PHP_EOL;
  echo rtrim('mechanism ' . _str($mech) . ' completed'), PHP_EOL;
}
