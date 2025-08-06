<?php
ini_set('memory_limit', '-1');
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
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
function make_hash_map() {
  return ['entries' => []];
}
function hm_len($m) {
  return _len($m['entries']);
}
function hm_set($m, $key, $value) {
  $entries = $m['entries'];
  $updated = false;
  $new_entries = [];
  $i = 0;
  while ($i < count($entries)) {
  $e = $entries[$i];
  if ($e['key'] == $key) {
  $new_entries = _append($new_entries, [$key => $key, $value => $value]);
  $updated = true;
} else {
  $new_entries = _append($new_entries, $e);
}
  $i = $i + 1;
};
  if (!$updated) {
  $new_entries = _append($new_entries, [$key => $key, $value => $value]);
}
  return ['entries' => $new_entries];
}
function hm_get($m, $key) {
  $i = 0;
  while ($i < _len($m['entries'])) {
  $e = $m['entries'][$i];
  if ($e['key'] == $key) {
  return ['found' => true, 'value' => $e['value']];
}
  $i = $i + 1;
};
  return ['found' => false, 'value' => ''];
}
function hm_del($m, $key) {
  $entries = $m['entries'];
  $new_entries = [];
  $removed = false;
  $i = 0;
  while ($i < count($entries)) {
  $e = $entries[$i];
  if ($e['key'] == $key) {
  $removed = true;
} else {
  $new_entries = _append($new_entries, $e);
}
  $i = $i + 1;
};
  if ($removed) {
  return ['map' => ['entries' => $new_entries], 'ok' => true];
}
  return ['map' => $m, 'ok' => false];
}
function test_add_items() {
  $h = make_hash_map();
  $h = hm_set($h, 'key_a', 'val_a');
  $h = hm_set($h, 'key_b', 'val_b');
  $a = hm_get($h, 'key_a');
  $b = hm_get($h, 'key_b');
  return hm_len($h) == 2 && $a['found'] && $b['found'] && $a['value'] == 'val_a' && $b['value'] == 'val_b';
}
function test_overwrite_items() {
  $h = make_hash_map();
  $h = hm_set($h, 'key_a', 'val_a');
  $h = hm_set($h, 'key_a', 'val_b');
  $a = hm_get($h, 'key_a');
  return hm_len($h) == 1 && $a['found'] && $a['value'] == 'val_b';
}
function test_delete_items() {
  $h = make_hash_map();
  $h = hm_set($h, 'key_a', 'val_a');
  $h = hm_set($h, 'key_b', 'val_b');
  $d1 = hm_del($h, 'key_a');
  $h = $d1['map'];
  $d2 = hm_del($h, 'key_b');
  $h = $d2['map'];
  $h = hm_set($h, 'key_a', 'val_a');
  $d3 = hm_del($h, 'key_a');
  $h = $d3['map'];
  return hm_len($h) == 0;
}
function test_access_absent_items() {
  $h = make_hash_map();
  $g1 = hm_get($h, 'key_a');
  $d1 = hm_del($h, 'key_a');
  $h = $d1['map'];
  $h = hm_set($h, 'key_a', 'val_a');
  $d2 = hm_del($h, 'key_a');
  $h = $d2['map'];
  $d3 = hm_del($h, 'key_a');
  $h = $d3['map'];
  $g2 = hm_get($h, 'key_a');
  return $g1['found'] == false && $d1['ok'] == false && $d2['ok'] && $d3['ok'] == false && $g2['found'] == false && hm_len($h) == 0;
}
function test_add_with_resize_up() {
  $h = make_hash_map();
  $i = 0;
  while ($i < 5) {
  $s = _str($i);
  $h = hm_set($h, $s, $s);
  $i = $i + 1;
};
  return hm_len($h) == 5;
}
function test_add_with_resize_down() {
  $h = make_hash_map();
  $i = 0;
  while ($i < 5) {
  $s = _str($i);
  $h = hm_set($h, $s, $s);
  $i = $i + 1;
};
  $j = 0;
  while ($j < 5) {
  $s = _str($j);
  $d = hm_del($h, $s);
  $h = $d['map'];
  $j = $j + 1;
};
  $h = hm_set($h, 'key_a', 'val_b');
  $a = hm_get($h, 'key_a');
  return hm_len($h) == 1 && $a['found'] && $a['value'] == 'val_b';
}
echo rtrim(json_encode(test_add_items(), 1344)), PHP_EOL;
echo rtrim(json_encode(test_overwrite_items(), 1344)), PHP_EOL;
echo rtrim(json_encode(test_delete_items(), 1344)), PHP_EOL;
echo rtrim(json_encode(test_access_absent_items(), 1344)), PHP_EOL;
echo rtrim(json_encode(test_add_with_resize_up(), 1344)), PHP_EOL;
echo rtrim(json_encode(test_add_with_resize_down(), 1344)), PHP_EOL;
echo rtrim((true ? 'true' : 'false')), PHP_EOL;
