<?php
function minPatches($nums, $n) { $miss = 1; $i = 0; $patches = 0; while ($miss <= $n) { if ($i < count($nums) && $nums[$i] <= $miss) $miss += $nums[$i++]; else { $miss += $miss; $patches++; } } return $patches; }
$data = preg_split('/\s+/', trim(stream_get_contents(STDIN))); if (count($data) && $data[0] !== '') { $idx = 0; $t = intval($data[$idx++]); $out = []; for ($tc = 0; $tc < $t; $tc++) { $size = intval($data[$idx++]); $nums = []; for ($i = 0; $i < $size; $i++) $nums[] = intval($data[$idx++]); $n = intval($data[$idx++]); $out[] = strval(minPatches($nums, $n)); } echo implode("\n\n", $out); }
?>
