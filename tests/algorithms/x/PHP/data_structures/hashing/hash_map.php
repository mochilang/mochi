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
function make_buckets($n) {
  global $hm;
  $buckets = null;
  $i = 0;
  while ($i < $n) {
  $buckets = _append($buckets, ['state' => 0, 'key' => 0, 'val' => 0]);
  $i = $i + 1;
};
  return $buckets;
}
function hashmap_new($initial_size) {
  global $hm;
  return ['buckets' => make_buckets($initial_size), 'len' => 0, 'cap_num' => 3, 'cap_den' => 4, 'initial_size' => $initial_size];
}
function bucket_index($hm, $key) {
  $ind = fmod($key, _len($hm['buckets']));
  if ($ind < 0) {
  $ind = $ind + _len($hm['buckets']);
}
  return $ind;
}
function next_index($hm, $ind) {
  return fmod(($ind + 1), _len($hm['buckets']));
}
function try_set(&$hm, $ind, $key, $val) {
  $buckets = $hm['buckets'];
  $b = $buckets[$ind];
  if ($b['state'] == 0 || $b['state'] == 2) {
  $buckets[$ind] = ['state' => 1, 'key' => $key, 'val' => $val];
  $hm['buckets'] = $buckets;
  $hm['len'] = $hm['len'] + 1;
  return true;
}
  if ($b['key'] == $key) {
  $buckets[$ind] = ['state' => 1, 'key' => $key, 'val' => $val];
  $hm['buckets'] = $buckets;
  return true;
}
  return false;
}
function is_full($hm) {
  $limit = _len($hm['buckets']) * $hm['cap_num'] / $hm['cap_den'];
  return $hm['len'] >= $limit;
}
function is_sparse($hm) {
  if (_len($hm['buckets']) <= $hm['initial_size']) {
  return false;
}
  $limit = _len($hm['buckets']) * $hm['cap_num'] / (2 * $hm['cap_den']);
  return $hm['len'] < $limit;
}
function resize(&$hm, $new_size) {
  $old = $hm['buckets'];
  $hm['buckets'] = make_buckets($new_size);
  $hm['len'] = 0;
  $i = 0;
  while ($i < count($old)) {
  $it = $old[$i];
  if ($it['state'] == 1) {
  add_item($hm, $it['key'], $it['val']);
}
  $i = $i + 1;
};
}
function size_up(&$hm) {
  resize($hm, _len($hm['buckets']) * 2);
}
function size_down(&$hm) {
  resize($hm, _len($hm['buckets']) / 2);
}
function add_item($hm, $key, $val) {
  $ind = bucket_index($hm, $key);
  $i = 0;
  while ($i < _len($hm['buckets'])) {
  if (try_set($hm, $ind, $key, $val)) {
  break;
}
  $ind = next_index($hm, $ind);
  $i = $i + 1;
};
}
function hashmap_set(&$hm, $key, $val) {
  if (is_full($hm)) {
  size_up($hm);
}
  add_item($hm, $key, $val);
}
function hashmap_get($hm, $key) {
  $buckets = $hm['buckets'];
  $ind = bucket_index($hm, $key);
  $i = 0;
  while ($i < count($buckets)) {
  $it = $buckets[$ind];
  if ($it['state'] == 0) {
  break;
}
  if ($it['state'] == 1 && $it['key'] == $key) {
  return $it['val'];
}
  $ind = next_index($hm, $ind);
  $i = $i + 1;
};
  return 0;
}
function hashmap_del(&$hm, $key) {
  $buckets = $hm['buckets'];
  $ind = bucket_index($hm, $key);
  $i = 0;
  while ($i < count($buckets)) {
  $it = $buckets[$ind];
  if ($it['state'] == 0) {
  echo rtrim('KeyError: ' . _str($key)), PHP_EOL;
  return;
}
  if ($it['state'] == 1 && $it['key'] == $key) {
  $buckets[$ind] = ['state' => 2, 'key' => 0, 'val' => 0];
  $hm['buckets'] = $buckets;
  $hm['len'] = $hm['len'] - 1;
  break;
}
  $ind = next_index($hm, $ind);
  $i = $i + 1;
};
  if (is_sparse($hm)) {
  size_down($hm);
}
}
function hashmap_len($hm) {
  return $hm['len'];
}
function hashmap_repr($hm) {
  $out = 'HashMap(';
  $first = true;
  $i = 0;
  while ($i < _len($hm['buckets'])) {
  $b = $hm['buckets'][$i];
  if ($b['state'] == 1) {
  if (!$first) {
  $out = $out . ', ';
} else {
  $first = false;
};
  $out = $out . _str($b['key']) . ': ' . _str($b['val']);
}
  $i = $i + 1;
};
  $out = $out . ')';
  return $out;
}
$hm = hashmap_new(5);
hashmap_set($hm, 1, 10);
hashmap_set($hm, 2, 20);
hashmap_set($hm, 3, 30);
echo rtrim(hashmap_repr($hm)), PHP_EOL;
echo rtrim(_str(hashmap_get($hm, 2))), PHP_EOL;
hashmap_del($hm, 1);
echo rtrim(hashmap_repr($hm)), PHP_EOL;
echo rtrim(_str(hashmap_len($hm))), PHP_EOL;
