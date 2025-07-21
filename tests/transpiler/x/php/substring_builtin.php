<?php
echo rtrim((is_float(substr("mochi", 1, 4 - 1)) ? sprintf("%.15f", substr("mochi", 1, 4 - 1)) : substr("mochi", 1, 4 - 1))), PHP_EOL;
?>
