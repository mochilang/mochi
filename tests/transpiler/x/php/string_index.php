<?php
$s = "mochi";
echo rtrim((is_float(substr($s, 1, 1 + 1 - 1)) ? sprintf("%.15f", substr($s, 1, 1 + 1 - 1)) : substr($s, 1, 1 + 1 - 1))), PHP_EOL;
?>
