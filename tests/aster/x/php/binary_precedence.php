<?php
echo (is_float(1 + 2 * 3) ? json_encode(1 + 2 * 3, 1344) : 1 + 2 * 3), PHP_EOL;
echo (is_float((1 + 2) * 3) ? json_encode((1 + 2) * 3, 1344) : (1 + 2) * 3), PHP_EOL;
echo (is_float(2 * 3 + 1) ? json_encode(2 * 3 + 1, 1344) : 2 * 3 + 1), PHP_EOL;
echo (is_float(2 * (3 + 1)) ? json_encode(2 * (3 + 1), 1344) : 2 * (3 + 1)), PHP_EOL;
