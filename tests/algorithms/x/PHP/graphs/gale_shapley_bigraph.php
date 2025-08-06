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
function index_of($xs, $x) {
  global $donor_pref, $recipient_pref;
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function remove_item($xs, $x) {
  global $donor_pref, $recipient_pref;
  $res = [];
  $removed = false;
  $i = 0;
  while ($i < count($xs)) {
  if (!$removed && $xs[$i] == $x) {
  $removed = true;
} else {
  $res = _append($res, $xs[$i]);
}
  $i = $i + 1;
};
  return $res;
}
function stable_matching($donor_pref, $recipient_pref) {
  if (count($donor_pref) != count($recipient_pref)) {
  $panic('unequal groups');
}
  $n = count($donor_pref);
  $unmatched = [];
  $i = 0;
  while ($i < $n) {
  $unmatched = _append($unmatched, $i);
  $i = $i + 1;
};
  $donor_record = [];
  $i = 0;
  while ($i < $n) {
  $donor_record = _append($donor_record, -1);
  $i = $i + 1;
};
  $rec_record = [];
  $i = 0;
  while ($i < $n) {
  $rec_record = _append($rec_record, -1);
  $i = $i + 1;
};
  $num_donations = [];
  $i = 0;
  while ($i < $n) {
  $num_donations = _append($num_donations, 0);
  $i = $i + 1;
};
  while (count($unmatched) > 0) {
  $donor = $unmatched[0];
  $donor_preference = $donor_pref[$donor];
  $recipient = $donor_preference[$num_donations[$donor]];
  $num_donations[$donor] = $num_donations[$donor] + 1;
  $rec_preference = $recipient_pref[$recipient];
  $prev_donor = $rec_record[$recipient];
  if ($prev_donor != 0 - 1) {
  $prev_index = index_of($rec_preference, $prev_donor);
  $new_index = index_of($rec_preference, $donor);
  if ($prev_index > $new_index) {
  $rec_record[$recipient] = $donor;
  $donor_record[$donor] = $recipient;
  $unmatched = _append($unmatched, $prev_donor);
  $unmatched = remove_item($unmatched, $donor);
};
} else {
  $rec_record[$recipient] = $donor;
  $donor_record[$donor] = $recipient;
  $unmatched = remove_item($unmatched, $donor);
}
};
  return $donor_record;
}
$donor_pref = [[0, 1, 3, 2], [0, 2, 3, 1], [1, 0, 2, 3], [0, 3, 1, 2]];
$recipient_pref = [[3, 1, 2, 0], [3, 1, 0, 2], [0, 3, 1, 2], [1, 0, 3, 2]];
echo rtrim(_str(stable_matching($donor_pref, $recipient_pref))), PHP_EOL;
