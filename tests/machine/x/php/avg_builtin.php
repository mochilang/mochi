<?php
_print(_avg([1, 2, 3]));
function _avg($v) {
    if (is_array($v) && array_key_exists('items', $v)) {
        $v = $v['items'];
    } elseif (is_object($v) && property_exists($v, 'items')) {
        $v = $v->items;
    }
    if (!is_array($v)) {
        throw new Exception('avg() expects list or group');
    }
    if (!$v) return 0;
    $sum = 0;
    foreach ($v as $it) {
        if (is_int($it) || is_float($it)) {
            $sum += $it;
        } else {
            throw new Exception('avg() expects numbers');
        }
    }
    return $sum / count($v);
}

function _print(...$args) {
    $first = true;
    foreach ($args as $a) {
        if (!$first) echo ' ';
        $first = false;
        if (is_array($a)) {
            if (array_is_list($a)) {
                if ($a && is_array($a[0])) {
                    $parts = [];
                    foreach ($a as $sub) {
                        if (is_array($sub)) {
                            $parts[] = '[' . implode(' ', $sub) . ']';
                        } else {
                            $parts[] = strval($sub);
                        }
                    }
                    echo implode(' ', $parts);
                } else {
                    echo '[' . implode(' ', array_map('strval', $a)) . ']';
                }
            } else {
                echo json_encode($a);
            }
        } else {
            echo strval($a);
        }
    }
    echo PHP_EOL;
}
?>
