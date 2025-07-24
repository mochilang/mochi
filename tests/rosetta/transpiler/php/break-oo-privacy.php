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
function examineAndModify(&$f) {
  global $anotherExample, $obj;
  echo rtrim(' v: {' . _str($f['Exported']) . ' ' . _str($f['unexported']) . '} = {' . _str($f['Exported']) . ' ' . _str($f['unexported']) . '}'), PHP_EOL;
  echo rtrim('    Idx Name       Type CanSet'), PHP_EOL;
  echo rtrim('     0: Exported   int  true'), PHP_EOL;
  echo rtrim('     1: unexported int  false'), PHP_EOL;
  $f['Exported'] = 16;
  $f['unexported'] = 44;
  echo rtrim('  modified unexported field via unsafe'), PHP_EOL;
  return $f;
}
function anotherExample() {
  global $examineAndModify, $obj;
  echo rtrim('bufio.ReadByte returned error: unsafely injected error value into bufio inner workings'), PHP_EOL;
}
$obj = ['Exported' => 12, 'unexported' => 42];
echo rtrim('obj: {' . _str($obj['Exported']) . ' ' . _str($obj['unexported']) . '}'), PHP_EOL;
$obj = examineAndModify($obj);
echo rtrim('obj: {' . _str($obj['Exported']) . ' ' . _str($obj['unexported']) . '}'), PHP_EOL;
anotherExample();
