<?php
function pow2($n) {
    $v = 1;
    $i = 0;
    while ($i < $n) {
        $v = $v * 2;
        $i = $i + 1;
    }
    return $v;
}
function lshift($x, $n) {
    return $x * pow2($n);
}
function rshift($x, $n) {
    return intdiv($x, pow2($n));
}
class Writer {
    public $order;
    public $bits;
    public $nbits;
    public $data;
    public function __construct($fields = []) {
        $this->order = $fields['order'] ?? null;
        $this->bits = $fields['bits'] ?? null;
        $this->nbits = $fields['nbits'] ?? null;
        $this->data = $fields['data'] ?? null;
    }
}
function NewWriter($order) {
    return new Writer([
    'order' => $order,
    'bits' => 0,
    'nbits' => 0,
    'data' => []
]);
}
function writeBitsLSB($w, $c, $width) {
    $w->bits = $w->bits + lshift($c, $w->nbits);
    $w->nbits = $w->nbits + $width;
    while ($w->nbits >= 8) {
        $b = $w->bits % 256;
        $w->data = array_merge($w->data, [$b]);
        $w->bits = rshift($w->bits, 8);
        $w->nbits = $w->nbits - 8;
    }
    return $w;
}
function writeBitsMSB($w, $c, $width) {
    $w->bits = $w->bits + lshift($c, 32 - $width - $w->nbits);
    $w->nbits = $w->nbits + $width;
    while ($w->nbits >= 8) {
        $b = rshift($w->bits, 24) % 256;
        $w->data = array_merge($w->data, [$b]);
        $w->bits = ($w->bits % pow2(24)) * 256;
        $w->nbits = $w->nbits - 8;
    }
    return $w;
}
function WriteBits($w, $c, $width) {
    if ($w->order == "LSB") {
        return writeBitsLSB($w, $c, $width);
    }
    return writeBitsMSB($w, $c, $width);
}
function CloseWriter($w) {
    if ($w->nbits > 0) {
        if ($w->order == "MSB") {
            $w->bits = rshift($w->bits, 24);
        }
        $w->data = array_merge($w->data, [$w->bits % 256]);
    }
    $w->bits = 0;
    $w->nbits = 0;
    return $w;
}
class Reader {
    public $order;
    public $data;
    public $idx;
    public $bits;
    public $nbits;
    public function __construct($fields = []) {
        $this->order = $fields['order'] ?? null;
        $this->data = $fields['data'] ?? null;
        $this->idx = $fields['idx'] ?? null;
        $this->bits = $fields['bits'] ?? null;
        $this->nbits = $fields['nbits'] ?? null;
    }
}
function NewReader($data, $order) {
    return new Reader([
    'order' => $order,
    'data' => $data,
    'idx' => 0,
    'bits' => 0,
    'nbits' => 0
]);
}
function readBitsLSB($r, $width) {
    while ($r->nbits < $width) {
        if ($r->idx >= count($r->data)) {
            return ["val" => 0, "eof" => true];
        }
        $b = $r->data[$r->idx];
        $r->idx = $r->idx + 1;
        $r->bits = $r->bits + lshift($b, $r->nbits);
        $r->nbits = $r->nbits + 8;
    }
    $mask = pow2($width) - 1;
    $out = $r->bits % ($mask + 1);
    $r->bits = rshift($r->bits, $width);
    $r->nbits = $r->nbits - $width;
    return ["val" => $out, "eof" => false];
}
function readBitsMSB($r, $width) {
    while ($r->nbits < $width) {
        if ($r->idx >= count($r->data)) {
            return ["val" => 0, "eof" => true];
        }
        $b = $r->data[$r->idx];
        $r->idx = $r->idx + 1;
        $r->bits = $r->bits + lshift($b, 24 - $r->nbits);
        $r->nbits = $r->nbits + 8;
    }
    $out = rshift($r->bits, 32 - $width);
    $r->bits = ($r->bits * pow2($width)) % pow2(32);
    $r->nbits = $r->nbits - $width;
    return ["val" => $out, "eof" => false];
}
function ReadBits($r, $width) {
    if ($r->order == "LSB") {
        return readBitsLSB($r, $width);
    }
    return readBitsMSB($r, $width);
}
function toBinary($n, $bits) {
    $b = "";
    $val = $n;
    $i = 0;
    while ($i < $bits) {
        $b = strval($val % 2) . $b;
        $val = $val / 2;
        $i = $i + 1;
    }
    return $b;
}
function bytesToBits($bs) {
    $out = "[";
    $i = 0;
    while ($i < count($bs)) {
        $out = $out . toBinary($bs[$i], 8);
        if ($i + 1 < count($bs)) {
            $out = $out . " ";
        }
        $i = $i + 1;
    }
    $out = $out . "]";
    return $out;
}
function bytesToHex($bs) {
    $digits = "0123456789ABCDEF";
    $out = "";
    $i = 0;
    while ($i < count($bs)) {
        $b = $bs[$i];
        $hi = $b / 16;
        $lo = $b % 16;
        $out = $out + array_slice($digits, $hi, $hi + 1 - $hi) + array_slice($digits, $lo, $lo + 1 - $lo);
        if ($i + 1 < count($bs)) {
            $out = $out . " ";
        }
        $i = $i + 1;
    }
    return $out;
}
function _ord($ch) {
    $upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    $lower = "abcdefghijklmnopqrstuvwxyz";
    $idx = strpos($upper, $ch);
    if ($idx >= 0) {
        return 65 + $idx;
    }
    $idx = strpos($lower, $ch);
    if ($idx >= 0) {
        return 97 + $idx;
    }
    if ($ch >= "0" && $ch <= "9") {
        return 48 + parseIntStr($ch);
    }
    if ($ch == " ") {
        return 32;
    }
    if ($ch == ".") {
        return 46;
    }
    return 0;
}
function _chr($n) {
    $upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    $lower = "abcdefghijklmnopqrstuvwxyz";
    if ($n >= 65 && $n < 91) {
        return array_slice($upper, $n - 65, $n - 64 - $n - 65);
    }
    if ($n >= 97 && $n < 123) {
        return array_slice($lower, $n - 97, $n - 96 - $n - 97);
    }
    if ($n >= 48 && $n < 58) {
        $digits = "0123456789";
        return array_slice($digits, $n - 48, $n - 47 - $n - 48);
    }
    if ($n == 32) {
        return " ";
    }
    if ($n == 46) {
        return ".";
    }
    return "?";
}
function bytesOfStr($s) {
    $bs = [];
    $i = 0;
    while ($i < strlen($s)) {
        $bs = array_merge($bs, [_ord(substr($s, $i, $i + 1 - $i))]);
        $i = $i + 1;
    }
    return $bs;
}
function bytesToDec($bs) {
    $out = "";
    $i = 0;
    while ($i < count($bs)) {
        $out = $out . strval($bs[$i]);
        if ($i + 1 < count($bs)) {
            $out = $out . " ";
        }
        $i = $i + 1;
    }
    return $out;
}
function Example() {
    $message = "This is a test.";
    $msgBytes = bytesOfStr($message);
    echo "\"" . $message . "\" as bytes: " . bytesToDec($msgBytes), PHP_EOL;
    echo "    original bits: " . bytesToBits($msgBytes), PHP_EOL;
    $bw = NewWriter("MSB");
    $i = 0;
    while ($i < _len($msgBytes)) {
        $bw = WriteBits($bw, $msgBytes[$i], 7);
        $i = $i + 1;
    }
    $bw = CloseWriter($bw);
    echo "Written bitstream: " . bytesToBits($bw['data']), PHP_EOL;
    echo "Written bytes: " . bytesToHex($bw['data']), PHP_EOL;
    $br = NewReader($bw['data'], "MSB");
    $result = "";
    while (true) {
        $r = ReadBits($br, 7);
        if ($r["eof"]) {
            break;
        }
        $v = (int)($r["val"]);
        if ($v != 0) {
            $result = $result . _chr($v);
        }
    }
    echo "Read back as \"" . $result . "\"", PHP_EOL;
}
Example();
function _len($v) {
    if (is_array($v) && array_key_exists('items', $v)) {
        return count($v['items']);
    }
    if (is_object($v) && property_exists($v, 'items')) {
        return count($v->items);
    }
    if (is_array($v)) {
        return count($v);
    }
    if (is_string($v)) {
        return strlen($v);
    }
    return 0;
}
?>
