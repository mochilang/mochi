<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
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
$__start_mem = memory_get_usage();
$__start = _now();
  $PI = 3.141592653589793;
  function sinApprox($x) {
  global $PI, $airports;
  $term = $x;
  $sum = $x;
  $n = 1;
  while ($n <= 8) {
  $denom = floatval(((2 * $n) * (2 * $n + 1)));
  $term = -$term * $x * $x / $denom;
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
};
  function cosApprox($x) {
  global $PI, $airports;
  $term = 1.0;
  $sum = 1.0;
  $n = 1;
  while ($n <= 8) {
  $denom = floatval(((2 * $n - 1) * (2 * $n)));
  $term = -$term * $x * $x / $denom;
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
};
  function atanApprox($x) {
  global $PI, $airports;
  if ($x > 1.0) {
  return $PI / 2.0 - $x / ($x * $x + 0.28);
}
  if ($x < (-1.0)) {
  return -$PI / 2.0 - $x / ($x * $x + 0.28);
}
  return $x / (1.0 + 0.28 * $x * $x);
};
  function atan2Approx($y, $x) {
  global $PI, $airports;
  if ($x > 0.0) {
  $r = atanApprox($y / $x);
  return $r;
}
  if ($x < 0.0) {
  if ($y >= 0.0) {
  return atanApprox($y / $x) + $PI;
};
  return atanApprox($y / $x) - $PI;
}
  if ($y > 0.0) {
  return $PI / 2.0;
}
  if ($y < 0.0) {
  return -$PI / 2.0;
}
  return 0.0;
};
  function sqrtApprox($x) {
  global $PI, $airports;
  $guess = $x;
  $i = 0;
  while ($i < 10) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function rad($x) {
  global $PI, $airports;
  return $x * $PI / 180.0;
};
  function deg($x) {
  global $PI, $airports;
  return $x * 180.0 / $PI;
};
  function distance($lat1, $lon1, $lat2, $lon2) {
  global $PI, $airports;
  $phi1 = rad($lat1);
  $phi2 = rad($lat2);
  $dphi = rad($lat2 - $lat1);
  $dlambda = rad($lon2 - $lon1);
  $sdphi = sinApprox($dphi / 2);
  $sdlambda = sinApprox($dlambda / 2);
  $a = $sdphi * $sdphi + cosApprox($phi1) * cosApprox($phi2) * $sdlambda * $sdlambda;
  $c = 2 * atan2Approx(sqrtApprox($a), sqrtApprox(1 - $a));
  return (6371.0 / 1.852) * $c;
};
  function bearing($lat1, $lon1, $lat2, $lon2) {
  global $PI, $airports;
  $phi1 = rad($lat1);
  $phi2 = rad($lat2);
  $dl = rad($lon2 - $lon1);
  $y = sinApprox($dl) * cosApprox($phi2);
  $x = cosApprox($phi1) * sinApprox($phi2) - sinApprox($phi1) * cosApprox($phi2) * cosApprox($dl);
  $br = deg(atan2Approx($y, $x));
  if ($br < 0) {
  $br = $br + 360;
}
  return $br;
};
  $airports = [['name' => 'Koksijde Air Base', 'country' => 'Belgium', 'icao' => 'EBFN', 'lat' => 51.090301513671875, 'lon' => 2.652780055999756], ['name' => 'Ostend-Bruges International Airport', 'country' => 'Belgium', 'icao' => 'EBOS', 'lat' => 51.198898315399994, 'lon' => 2.8622200489], ['name' => 'Kent International Airport', 'country' => 'United Kingdom', 'icao' => 'EGMH', 'lat' => 51.342201, 'lon' => 1.34611], ['name' => 'Calais-Dunkerque Airport', 'country' => 'France', 'icao' => 'LFAC', 'lat' => 50.962100982666016, 'lon' => 1.954759955406189], ['name' => 'Westkapelle heliport', 'country' => 'Belgium', 'icao' => 'EBKW', 'lat' => 51.32222366333, 'lon' => 3.2930560112], ['name' => 'Lympne Airport', 'country' => 'United Kingdom', 'icao' => 'EGMK', 'lat' => 51.08, 'lon' => 1.013], ['name' => 'Ursel Air Base', 'country' => 'Belgium', 'icao' => 'EBUL', 'lat' => 51.14419937133789, 'lon' => 3.475559949874878], ['name' => 'Southend Airport', 'country' => 'United Kingdom', 'icao' => 'EGMC', 'lat' => 51.5713996887207, 'lon' => 0.6955559849739075], ['name' => 'Merville-Calonne Airport', 'country' => 'France', 'icao' => 'LFQT', 'lat' => 50.61840057373047, 'lon' => 2.642240047454834], ['name' => 'Wevelgem Airport', 'country' => 'Belgium', 'icao' => 'EBKT', 'lat' => 50.817199707, 'lon' => 3.20472002029], ['name' => 'Midden-Zeeland Airport', 'country' => 'Netherlands', 'icao' => 'EHMZ', 'lat' => 51.5121994019, 'lon' => 3.73111009598], ['name' => 'Lydd Airport', 'country' => 'United Kingdom', 'icao' => 'EGMD', 'lat' => 50.95610046386719, 'lon' => 0.9391670227050781], ['name' => 'RAF Wattisham', 'country' => 'United Kingdom', 'icao' => 'EGUW', 'lat' => 52.1273002625, 'lon' => 0.956264019012], ['name' => 'Beccles Airport', 'country' => 'United Kingdom', 'icao' => 'EGSM', 'lat' => 52.435298919699996, 'lon' => 1.6183300018300002], ['name' => 'Lille/Marcq-en-Baroeul Airport', 'country' => 'France', 'icao' => 'LFQO', 'lat' => 50.687198638916016, 'lon' => 3.0755600929260254], ['name' => 'Lashenden (Headcorn) Airfield', 'country' => 'United Kingdom', 'icao' => 'EGKH', 'lat' => 51.156898, 'lon' => 0.641667], ['name' => 'Le Touquet-Côte d\'Opale Airport', 'country' => 'France', 'icao' => 'LFAT', 'lat' => 50.517398834228516, 'lon' => 1.6205899715423584], ['name' => 'Rochester Airport', 'country' => 'United Kingdom', 'icao' => 'EGTO', 'lat' => 51.351898193359375, 'lon' => 0.5033329725265503], ['name' => 'Lille-Lesquin Airport', 'country' => 'France', 'icao' => 'LFQQ', 'lat' => 50.563332, 'lon' => 3.086886], ['name' => 'Thurrock Airfield', 'country' => 'United Kingdom', 'icao' => 'EGMT', 'lat' => 51.537505, 'lon' => 0.367634]];
  function mochi_floor($x) {
  global $PI, $airports;
  $i = intval($x);
  if ((floatval($i)) > $x) {
  $i = $i - 1;
}
  return floatval($i);
};
  function pow10($n) {
  global $PI, $airports;
  $p = 1.0;
  $i = 0;
  while ($i < $n) {
  $p = $p * 10.0;
  $i = $i + 1;
};
  return $p;
};
  function mochi_round($x, $n) {
  global $PI, $airports;
  $m = pow10($n);
  return mochi_floor($x * $m + 0.5) / $m;
};
  function sortByDistance($xs) {
  global $PI, $airports;
  $arr = $xs;
  $i = 1;
  while ($i < count($arr)) {
  $j = $i;
  while ($j > 0 && $arr[$j - 1][0] > $arr[$j][0]) {
  $tmp = $arr[$j - 1];
  $arr[$j - 1] = $arr[$j];
  $arr[$j] = $tmp;
  $j = $j - 1;
};
  $i = $i + 1;
};
  return $arr;
};
  function main() {
  global $PI, $airports;
  $planeLat = 51.514669;
  $planeLon = 2.198581;
  $results = [];
  foreach ($airports as $ap) {
  $d = distance($planeLat, $planeLon, $ap['lat'], $ap['lon']);
  $b = bearing($planeLat, $planeLon, $ap['lat'], $ap['lon']);
  $results = array_merge($results, [[$d, $b, $ap]]);
};
  $results = sortByDistance($results);
  echo rtrim('Distance Bearing ICAO Country               Airport'), PHP_EOL;
  echo rtrim('--------------------------------------------------------------'), PHP_EOL;
  $i = 0;
  while ($i < count($results)) {
  $r = $results[$i];
  $ap = $r[2];
  $dist = $r[0];
  $bear = $r[1];
  $line = _str(mochi_round($dist, 1)) . '\t' . _str(mochi_round($bear, 0)) . '\t' . $ap['icao'] . '\t' . $ap['country'] . ' ' . $ap['name'];
  echo rtrim(json_encode($line, 1344)), PHP_EOL;
  $i = $i + 1;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
