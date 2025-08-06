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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$abc = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
$low_abc = 'abcdefghijklmnopqrstuvwxyz';
$rotor1 = 'EGZWVONAHDCLFQMSIPJBYUKXTR';
$rotor2 = 'FOBHMDKEXQNRAULPGSJVTYICZW';
$rotor3 = 'ZJXESIUQLHAVRMDOYGTNFWPBKC';
$rotor4 = 'RMDJXFUWGISLHVTCQNKYPBEZOA';
$rotor5 = 'SGLCPQWZHKXAREONTFBVIYJUDM';
$rotor6 = 'HVSICLTYKQUBXDWAJZOMFGPREN';
$rotor7 = 'RZWQHFMVDBKICJLNTUXAGYPSOE';
$rotor8 = 'LFKIJODBEGAMQPXVUHYSTCZRWN';
$rotor9 = 'KOAEGVDHXPQZMLFTYWJNBRCIUS';
$reflector_pairs = ['AN', 'BO', 'CP', 'DQ', 'ER', 'FS', 'GT', 'HU', 'IV', 'JW', 'KX', 'LY', 'MZ'];
function list_contains($xs, $x) {
  global $abc, $low_abc, $rotor1, $rotor2, $rotor3, $rotor4, $rotor5, $rotor6, $rotor7, $rotor8, $rotor9, $reflector_pairs;
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  return true;
}
  $i = $i + 1;
};
  return false;
}
function index_in_string($s, $ch) {
  global $abc, $low_abc, $rotor1, $rotor2, $rotor3, $rotor4, $rotor5, $rotor6, $rotor7, $rotor8, $rotor9, $reflector_pairs;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function contains_char($s, $ch) {
  global $abc, $low_abc, $rotor1, $rotor2, $rotor3, $rotor4, $rotor5, $rotor6, $rotor7, $rotor8, $rotor9, $reflector_pairs;
  return index_in_string($s, $ch) >= 0;
}
function to_uppercase($s) {
  global $abc, $low_abc, $rotor1, $rotor2, $rotor3, $rotor4, $rotor5, $rotor6, $rotor7, $rotor8, $rotor9, $reflector_pairs;
  $res = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  $idx = index_in_string($low_abc, $ch);
  if ($idx >= 0) {
  $res = $res . substr($abc, $idx, $idx + 1 - $idx);
} else {
  $res = $res . $ch;
}
  $i = $i + 1;
};
  return $res;
}
function plugboard_map($pb, $ch) {
  global $abc, $low_abc, $rotor1, $rotor2, $rotor3, $rotor4, $rotor5, $rotor6, $rotor7, $rotor8, $rotor9, $reflector_pairs;
  $i = 0;
  while ($i < count($pb)) {
  $pair = $pb[$i];
  $a = substr($pair, 0, 1 - 0);
  $b = substr($pair, 1, 2 - 1);
  if ($ch == $a) {
  return $b;
}
  if ($ch == $b) {
  return $a;
}
  $i = $i + 1;
};
  return $ch;
}
function reflector_map($ch) {
  global $abc, $low_abc, $rotor1, $rotor2, $rotor3, $rotor4, $rotor5, $rotor6, $rotor7, $rotor8, $rotor9, $reflector_pairs;
  $i = 0;
  while ($i < count($reflector_pairs)) {
  $pair = $reflector_pairs[$i];
  $a = substr($pair, 0, 1 - 0);
  $b = substr($pair, 1, 2 - 1);
  if ($ch == $a) {
  return $b;
}
  if ($ch == $b) {
  return $a;
}
  $i = $i + 1;
};
  return $ch;
}
function count_unique($xs) {
  global $abc, $low_abc, $rotor1, $rotor2, $rotor3, $rotor4, $rotor5, $rotor6, $rotor7, $rotor8, $rotor9, $reflector_pairs;
  $unique = [];
  $i = 0;
  while ($i < count($xs)) {
  if (!list_contains($unique, $xs[$i])) {
  $unique = _append($unique, $xs[$i]);
}
  $i = $i + 1;
};
  return count($unique);
}
function build_plugboard($pbstring) {
  global $abc, $low_abc, $rotor1, $rotor2, $rotor3, $rotor4, $rotor5, $rotor6, $rotor7, $rotor8, $rotor9, $reflector_pairs;
  if (strlen($pbstring) == 0) {
  return [];
}
  if (fmod(strlen($pbstring), 2) != 0) {
  $panic('Odd number of symbols(' . _str(strlen($pbstring)) . ')');
}
  $pbstring_nospace = '';
  $i = 0;
  while ($i < strlen($pbstring)) {
  $ch = substr($pbstring, $i, $i + 1 - $i);
  if ($ch != ' ') {
  $pbstring_nospace = $pbstring_nospace . $ch;
}
  $i = $i + 1;
};
  $seen = [];
  $i = 0;
  while ($i < strlen($pbstring_nospace)) {
  $ch = substr($pbstring_nospace, $i, $i + 1 - $i);
  if (!contains_char($abc, $ch)) {
  $panic('\'' . $ch . '\' not in list of symbols');
}
  if (list_contains($seen, $ch)) {
  $panic('Duplicate symbol(' . $ch . ')');
}
  $seen = _append($seen, $ch);
  $i = $i + 1;
};
  $pb = [];
  $i = 0;
  while ($i < strlen($pbstring_nospace) - 1) {
  $a = substr($pbstring_nospace, $i, $i + 1 - $i);
  $b = substr($pbstring_nospace, $i + 1, $i + 2 - ($i + 1));
  $pb = _append($pb, $a . $b);
  $i = $i + 2;
};
  return $pb;
}
function validator($rotpos, $rotsel, $pb) {
  global $abc, $low_abc, $rotor1, $rotor2, $rotor3, $rotor4, $rotor5, $rotor6, $rotor7, $rotor8, $rotor9, $reflector_pairs;
  if (count_unique($rotsel) < 3) {
  $panic('Please use 3 unique rotors (not ' . _str(count_unique($rotsel)) . ')');
}
  if (count($rotpos) != 3) {
  $panic('Rotor position must have 3 values');
}
  $r1 = $rotpos[0];
  $r2 = $rotpos[1];
  $r3 = $rotpos[2];
  if (!(0 < $r1 && $r1 <= strlen($abc))) {
  $panic('First rotor position is not within range of 1..26 (' . _str($r1) . ')');
}
  if (!(0 < $r2 && $r2 <= strlen($abc))) {
  $panic('Second rotor position is not within range of 1..26 (' . _str($r2) . ')');
}
  if (!(0 < $r3 && $r3 <= strlen($abc))) {
  $panic('Third rotor position is not within range of 1..26 (' . _str($r3) . ')');
}
}
function enigma($text, $rotor_position, $rotor_selection, $plugb) {
  global $abc, $low_abc, $rotor1, $rotor2, $rotor3, $rotor4, $rotor5, $rotor6, $rotor7, $rotor8, $rotor9, $reflector_pairs;
  $up_text = to_uppercase($text);
  $up_pb = to_uppercase($plugb);
  validator($rotor_position, $rotor_selection, $up_pb);
  $plugboard = build_plugboard($up_pb);
  $rotorpos1 = $rotor_position[0] - 1;
  $rotorpos2 = $rotor_position[1] - 1;
  $rotorpos3 = $rotor_position[2] - 1;
  $rotor_a = $rotor_selection[0];
  $rotor_b = $rotor_selection[1];
  $rotor_c = $rotor_selection[2];
  $result = '';
  $i = 0;
  while ($i < strlen($up_text)) {
  $symbol = substr($up_text, $i, $i + 1 - $i);
  if (contains_char($abc, $symbol)) {
  $symbol = plugboard_map($plugboard, $symbol);
  $index = index_in_string($abc, $symbol) + $rotorpos1;
  $symbol = substr($rotor_a, fmod($index, strlen($abc)), fmod($index, strlen($abc)) + 1 - fmod($index, strlen($abc)));
  $index = index_in_string($abc, $symbol) + $rotorpos2;
  $symbol = substr($rotor_b, fmod($index, strlen($abc)), fmod($index, strlen($abc)) + 1 - fmod($index, strlen($abc)));
  $index = index_in_string($abc, $symbol) + $rotorpos3;
  $symbol = substr($rotor_c, fmod($index, strlen($abc)), fmod($index, strlen($abc)) + 1 - fmod($index, strlen($abc)));
  $symbol = reflector_map($symbol);
  $index = index_in_string($rotor_c, $symbol) - $rotorpos3;
  if ($index < 0) {
  $index = $index + strlen($abc);
};
  $symbol = substr($abc, $index, $index + 1 - $index);
  $index = index_in_string($rotor_b, $symbol) - $rotorpos2;
  if ($index < 0) {
  $index = $index + strlen($abc);
};
  $symbol = substr($abc, $index, $index + 1 - $index);
  $index = index_in_string($rotor_a, $symbol) - $rotorpos1;
  if ($index < 0) {
  $index = $index + strlen($abc);
};
  $symbol = substr($abc, $index, $index + 1 - $index);
  $symbol = plugboard_map($plugboard, $symbol);
  $rotorpos1 = $rotorpos1 + 1;
  if ($rotorpos1 >= strlen($abc)) {
  $rotorpos1 = 0;
  $rotorpos2 = $rotorpos2 + 1;
};
  if ($rotorpos2 >= strlen($abc)) {
  $rotorpos2 = 0;
  $rotorpos3 = $rotorpos3 + 1;
};
  if ($rotorpos3 >= strlen($abc)) {
  $rotorpos3 = 0;
};
}
  $result = $result . $symbol;
  $i = $i + 1;
};
  return $result;
}
function main() {
  global $abc, $low_abc, $rotor1, $rotor2, $rotor3, $rotor4, $rotor5, $rotor6, $rotor7, $rotor8, $rotor9, $reflector_pairs;
  $message = 'This is my Python script that emulates the Enigma machine from WWII.';
  $rotor_pos = [1, 1, 1];
  $pb = 'pictures';
  $rotor_sel = [$rotor2, $rotor4, $rotor8];
  $en = enigma($message, $rotor_pos, $rotor_sel, $pb);
  echo rtrim('Encrypted message: ' . $en), PHP_EOL;
  echo rtrim('Decrypted message: ' . enigma($en, $rotor_pos, $rotor_sel, $pb)), PHP_EOL;
}
main();
