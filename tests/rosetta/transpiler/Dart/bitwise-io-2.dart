// Generated by Mochi transpiler
String _substr(String s, int start, int end) {
  var n = s.length;
  if (start < 0) start += n;
  if (end < 0) end += n;
  if (start < 0) start = 0;
  if (start > n) start = n;
  if (end < 0) end = 0;
  if (end > n) end = n;
  if (start > end) start = end;
  return s.substring(start, end);
}

class Writer {
  String order;
  int bits;
  int nbits;
  List<int> data;
  Writer({required this.order, required this.bits, required this.nbits, required this.data});
}

class Reader {
  String order;
  List<int> data;
  int idx;
  int bits;
  int nbits;
  Reader({required this.order, required this.data, required this.idx, required this.bits, required this.nbits});
}

int pow2(int n) {
  int v = 1;
  int i = 0;
  while (i < n) {
    v = v * 2;
    i = i + 1;
  }
  return v;
}

int lshift(int x, int n) {
  return x * pow2(n);
}

int rshift(int x, int n) {
  return x ~/ pow2(n);
}

Writer NewWriter(String order) {
  return Writer(order: order, bits: 0, nbits: 0, data: []);
}

Writer writeBitsLSB(Writer w, int c, int width) {
  w.bits = w.bits + lshift(c, w.nbits);
  w.nbits = w.nbits + width;
  while (w.nbits >= 8) {
    int b = w.bits % 256;
    w.data = [...w.data, b];
    w.bits = rshift(w.bits, 8);
    w.nbits = w.nbits - 8;
  }
  return w;
}

Writer writeBitsMSB(Writer w, int c, int width) {
  w.bits = w.bits + lshift(c, 32 - width - w.nbits);
  w.nbits = w.nbits + width;
  while (w.nbits >= 8) {
    int b = rshift(w.bits, 24) % 256;
    w.data = [...w.data, b];
    w.bits = w.bits % pow2(24) * 256;
    w.nbits = w.nbits - 8;
  }
  return w;
}

Writer WriteBits(Writer w, int c, int width) {
  if (w.order == "LSB") {
    return writeBitsLSB(w, c, width);
  }
  return writeBitsMSB(w, c, width);
}

Writer CloseWriter(Writer w) {
  if (w.nbits > 0) {
    if (w.order == "MSB") {
    w.bits = rshift(w.bits, 24);
  };
    w.data = [...w.data, w.bits % 256];
  }
  w.bits = 0;
  w.nbits = 0;
  return w;
}

Reader NewReader(List<int> data, String order) {
  return Reader(order: order, data: data, idx: 0, bits: 0, nbits: 0);
}

Map<String, dynamic> readBitsLSB(Reader r, int width) {
  while (r.nbits < width) {
    if (r.idx >= r.data.length) {
    return {"val": 0, "eof": true};
  }
    int b = r.data[r.idx];
    r.idx = r.idx + 1;
    r.bits = r.bits + lshift(b, r.nbits);
    r.nbits = r.nbits + 8;
  }
  int mask = pow2(width) - 1;
  int out = r.bits % (mask + 1);
  r.bits = rshift(r.bits, width);
  r.nbits = r.nbits - width;
  return {"val": out, "eof": false};
}

Map<String, dynamic> readBitsMSB(Reader r, int width) {
  while (r.nbits < width) {
    if (r.idx >= r.data.length) {
    return {"val": 0, "eof": true};
  }
    int b = r.data[r.idx];
    r.idx = r.idx + 1;
    r.bits = r.bits + lshift(b, 24 - r.nbits);
    r.nbits = r.nbits + 8;
  }
  int out = rshift(r.bits, 32 - width);
  r.bits = r.bits * pow2(width) % pow2(32);
  r.nbits = r.nbits - width;
  return {"val": out, "eof": false};
}

Map<String, dynamic> ReadBits(Reader r, int width) {
  if (r.order == "LSB") {
    return readBitsLSB(r, width);
  }
  return readBitsMSB(r, width);
}

String toBinary(int n, int bits) {
  String b = "";
  int val = n;
  int i = 0;
  while (i < bits) {
    b = (val % 2).toString() + b;
    val = val ~/ 2;
    i = i + 1;
  }
  return b;
}

String bytesToBits(List<int> bs) {
  String out = "[";
  int i = 0;
  while (i < bs.length) {
    out = out + toBinary(bs[i], 8);
    if (i + 1 < bs.length) {
    out = out + " ";
  }
    i = i + 1;
  }
  out = out + "]";
  return out;
}

String bytesToHex(List<int> bs) {
  String digits = "0123456789ABCDEF";
  String out = "";
  int i = 0;
  while (i < bs.length) {
    int b = bs[i];
    int hi = b ~/ 16;
    int lo = b % 16;
    out = out + _substr(digits, hi, hi + 1) + _substr(digits, lo, lo + 1);
    if (i + 1 < bs.length) {
    out = out + " ";
  }
    i = i + 1;
  }
  return out;
}

int ord(String ch) {
  String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  String lower = "abcdefghijklmnopqrstuvwxyz";
  int idx = upper.indexOf(ch);
  if (idx >= 0) {
    return 65 + idx;
  }
  idx = lower.indexOf(ch);
  if (idx >= 0) {
    return 97 + idx;
  }
  if (ch.compareTo("0") >= 0 && ch.compareTo("9") <= 0) {
    return 48 + int.parse(ch);
  }
  if (ch == " ") {
    return 32;
  }
  if (ch == ".") {
    return 46;
  }
  return 0;
}

String chr(int n) {
  String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  String lower = "abcdefghijklmnopqrstuvwxyz";
  if (n >= 65 && n < 91) {
    return _substr(upper, n - 65, n - 64);
  }
  if (n >= 97 && n < 123) {
    return _substr(lower, n - 97, n - 96);
  }
  if (n >= 48 && n < 58) {
    String digits = "0123456789";
    return _substr(digits, n - 48, n - 47);
  }
  if (n == 32) {
    return " ";
  }
  if (n == 46) {
    return ".";
  }
  return "?";
}

List<int> bytesOfStr(String s) {
  List<int> bs = <int>[];
  int i = 0;
  while (i < s.length) {
    bs = [...bs, ord(_substr(s, i, i + 1))];
    i = i + 1;
  }
  return bs;
}

String bytesToDec(List<int> bs) {
  String out = "";
  int i = 0;
  while (i < bs.length) {
    out = out + (bs[i]).toString();
    if (i + 1 < bs.length) {
    out = out + " ";
  }
    i = i + 1;
  }
  return out;
}

void Example() {
  String message = "This is a test.";
  List<int> msgBytes = bytesOfStr(message);
  print("\"" + message + "\" as bytes: " + bytesToDec(msgBytes));
  print("    original bits: " + bytesToBits(msgBytes));
  Writer bw = NewWriter("MSB");
  int i = 0;
  while (i < msgBytes.length) {
    bw = WriteBits(bw, msgBytes[i], 7);
    i = i + 1;
  }
  bw = CloseWriter(bw);
  print("Written bitstream: " + bytesToBits(bw.data));
  print("Written bytes: " + bytesToHex(bw.data));
  Reader br = NewReader(bw.data, "MSB");
  String result = "";
  while (true) {
    Map<String, dynamic> r = ReadBits(br, 7);
    if (r["eof"]) {
    break;
  }
    int v = r["val"] as int;
    if (v != 0) {
    result = result + chr(v);
  }
  }
  print("Read back as \"" + result + "\"");
}

void main() {
  Example();
}
