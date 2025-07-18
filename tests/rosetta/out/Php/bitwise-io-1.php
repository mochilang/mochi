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
function toBinary($n, $bits) {
    $b = "";
    $val = $n;
    $i = 0;
    while ($i < $bits) {
        $b = strval($val % 2) . $b;
        $val = intdiv($val, 2);
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
function ExampleWriter_WriteBits() {
    $bw = NewWriter("MSB");
    $bw = WriteBits($bw, 15, 4);
    $bw = WriteBits($bw, 0, 1);
    $bw = WriteBits($bw, 19, 5);
    $bw = CloseWriter($bw);
    var_dump(bytesToBits($bw->data));
}
ExampleWriter_WriteBits();
?>
