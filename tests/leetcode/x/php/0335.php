<?php
function isSelfCrossing($x) { for ($i = 3; $i < count($x); $i++) { if ($x[$i] >= $x[$i-2] && $x[$i-1] <= $x[$i-3]) return true; if ($i >= 4 && $x[$i-1] == $x[$i-3] && $x[$i] + $x[$i-4] >= $x[$i-2]) return true; if ($i >= 5 && $x[$i-2] >= $x[$i-4] && $x[$i] + $x[$i-4] >= $x[$i-2] && $x[$i-1] <= $x[$i-3] && $x[$i-1] + $x[$i-5] >= $x[$i-3]) return true; } return false; }
$data = preg_split('/\s+/', trim(stream_get_contents(STDIN))); if (count($data) && $data[0] !== '') { $idx = 0; $t = intval($data[$idx++]); $out = []; for ($tc = 0; $tc < $t; $tc++) { $n = intval($data[$idx++]); $x = []; for ($i = 0; $i < $n; $i++) $x[] = intval($data[$idx++]); $out[] = isSelfCrossing($x) ? "true" : "false"; } echo implode("\n\n", $out); }
?>
