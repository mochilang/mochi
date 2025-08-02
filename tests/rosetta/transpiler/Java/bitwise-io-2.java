public class Main {
    static class Writer {
        String order;
        int bits;
        int nbits;
        int[] data;
        Writer(String order, int bits, int nbits, int[] data) {
            this.order = order;
            this.bits = bits;
            this.nbits = nbits;
            this.data = data;
        }
        @Override public String toString() {
            return String.format("{'order': '%s', 'bits': %s, 'nbits': %s, 'data': %s}", String.valueOf(order), String.valueOf(bits), String.valueOf(nbits), String.valueOf(data));
        }
    }

    static class Reader {
        String order;
        int[] data;
        int idx;
        int bits;
        int nbits;
        Reader(String order, int[] data, int idx, int bits, int nbits) {
            this.order = order;
            this.data = data;
            this.idx = idx;
            this.bits = bits;
            this.nbits = nbits;
        }
        @Override public String toString() {
            return String.format("{'order': '%s', 'data': %s, 'idx': %s, 'bits': %s, 'nbits': %s}", String.valueOf(order), String.valueOf(data), String.valueOf(idx), String.valueOf(bits), String.valueOf(nbits));
        }
    }


    static int pow2(int n) {
        int v = 1;
        int i = 0;
        while (i < n) {
            v = v * 2;
            i = i + 1;
        }
        return v;
    }

    static int lshift(int x, int n) {
        return x * pow2(n);
    }

    static int rshift(int x, int n) {
        return x / pow2(n);
    }

    static Writer NewWriter(String order) {
        return new Writer(order, 0, 0, new int[]{});
    }

    static Writer writeBitsLSB(Writer w, int c, int width) {
w.bits = w.bits + lshift(c, w.nbits);
w.nbits = w.nbits + width;
        while (w.nbits >= 8) {
            int b = Math.floorMod(w.bits, 256);
w.data = java.util.stream.IntStream.concat(java.util.Arrays.stream(w.data), java.util.stream.IntStream.of(b)).toArray();
w.bits = rshift(w.bits, 8);
w.nbits = w.nbits - 8;
        }
        return w;
    }

    static Writer writeBitsMSB(Writer w, int c, int width) {
w.bits = w.bits + lshift(c, 32 - width - w.nbits);
w.nbits = w.nbits + width;
        while (w.nbits >= 8) {
            int b_1 = Math.floorMod(rshift(w.bits, 24), 256);
w.data = java.util.stream.IntStream.concat(java.util.Arrays.stream(w.data), java.util.stream.IntStream.of(b_1)).toArray();
w.bits = ((Number)((_modPow2(w.bits, 24)))).intValue() * 256;
w.nbits = w.nbits - 8;
        }
        return w;
    }

    static Writer WriteBits(Writer w, int c, int width) {
        if ((w.order.equals("LSB"))) {
            return writeBitsLSB(w, c, width);
        }
        return writeBitsMSB(w, c, width);
    }

    static Writer CloseWriter(Writer w) {
        if (w.nbits > 0) {
            if ((w.order.equals("MSB"))) {
w.bits = rshift(w.bits, 24);
            }
w.data = java.util.stream.IntStream.concat(java.util.Arrays.stream(w.data), java.util.stream.IntStream.of(Math.floorMod(w.bits, 256))).toArray();
        }
w.bits = 0;
w.nbits = 0;
        return w;
    }

    static Reader NewReader(int[] data, String order) {
        return new Reader(order, data, 0, 0, 0);
    }

    static java.util.Map<String,Object> readBitsLSB(Reader r, int width) {
        while (r.nbits < width) {
            if (r.idx >= r.data.length) {
                return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("val", 0), java.util.Map.entry("eof", true)));
            }
            int b_2 = r.data[r.idx];
r.idx = r.idx + 1;
r.bits = r.bits + lshift(b_2, r.nbits);
r.nbits = r.nbits + 8;
        }
        int mask = pow2(width) - 1;
        int out = Math.floorMod(r.bits, (mask + 1));
r.bits = rshift(r.bits, width);
r.nbits = r.nbits - width;
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("val", out), java.util.Map.entry("eof", false)));
    }

    static java.util.Map<String,Object> readBitsMSB(Reader r, int width) {
        while (r.nbits < width) {
            if (r.idx >= r.data.length) {
                return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("val", 0), java.util.Map.entry("eof", true)));
            }
            int b_3 = r.data[r.idx];
r.idx = r.idx + 1;
r.bits = r.bits + lshift(b_3, 24 - r.nbits);
r.nbits = r.nbits + 8;
        }
        int out_1 = rshift(r.bits, 32 - width);
r.bits = _modPow2((r.bits * pow2(width)), 32);
r.nbits = r.nbits - width;
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("val", out_1), java.util.Map.entry("eof", false)));
    }

    static java.util.Map<String,Object> ReadBits(Reader r, int width) {
        if ((r.order.equals("LSB"))) {
            return readBitsLSB(r, width);
        }
        return readBitsMSB(r, width);
    }

    static String toBinary(int n, int bits) {
        String b_4 = "";
        int val = n;
        int i_1 = 0;
        while (i_1 < bits) {
            b_4 = _p(Math.floorMod(val, 2)) + b_4;
            val = val / 2;
            i_1 = i_1 + 1;
        }
        return b_4;
    }

    static String bytesToBits(int[] bs) {
        String out_2 = "[";
        int i_2 = 0;
        while (i_2 < bs.length) {
            out_2 = out_2 + String.valueOf(toBinary(bs[i_2], 8));
            if (i_2 + 1 < bs.length) {
                out_2 = out_2 + " ";
            }
            i_2 = i_2 + 1;
        }
        out_2 = out_2 + "]";
        return out_2;
    }

    static String bytesToHex(int[] bs) {
        String digits = "0123456789ABCDEF";
        String out_3 = "";
        int i_3 = 0;
        while (i_3 < bs.length) {
            int b_5 = bs[i_3];
            int hi = b_5 / 16;
            int lo = Math.floorMod(b_5, 16);
            out_3 = out_3 + digits.substring(hi, hi + 1) + digits.substring(lo, lo + 1);
            if (i_3 + 1 < bs.length) {
                out_3 = out_3 + " ";
            }
            i_3 = i_3 + 1;
        }
        return out_3;
    }

    static int ord(String ch) {
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower = "abcdefghijklmnopqrstuvwxyz";
        int idx = ((Number)(upper.indexOf(ch))).intValue();
        if (idx >= 0) {
            return 65 + idx;
        }
        idx = ((Number)(lower.indexOf(ch))).intValue();
        if (idx >= 0) {
            return 97 + idx;
        }
        if ((ch.compareTo("0") >= 0) && (ch.compareTo("9") <= 0)) {
            return 48 + Integer.parseInt(ch);
        }
        if ((ch.equals(" "))) {
            return 32;
        }
        if ((ch.equals("."))) {
            return 46;
        }
        return 0;
    }

    static String chr(int n) {
        String upper_1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower_1 = "abcdefghijklmnopqrstuvwxyz";
        if (n >= 65 && n < 91) {
            return upper_1.substring(n - 65, n - 64);
        }
        if (n >= 97 && n < 123) {
            return lower_1.substring(n - 97, n - 96);
        }
        if (n >= 48 && n < 58) {
            String digits_1 = "0123456789";
            return digits_1.substring(n - 48, n - 47);
        }
        if (n == 32) {
            return " ";
        }
        if (n == 46) {
            return ".";
        }
        return "?";
    }

    static int[] bytesOfStr(String s) {
        int[] bs = new int[]{};
        int i_4 = 0;
        while (i_4 < _runeLen(s)) {
            bs = java.util.stream.IntStream.concat(java.util.Arrays.stream(bs), java.util.stream.IntStream.of(ord(s.substring(i_4, i_4 + 1)))).toArray();
            i_4 = i_4 + 1;
        }
        return bs;
    }

    static String bytesToDec(int[] bs) {
        String out_4 = "";
        int i_5 = 0;
        while (i_5 < bs.length) {
            out_4 = out_4 + _p(_geti(bs, i_5));
            if (i_5 + 1 < bs.length) {
                out_4 = out_4 + " ";
            }
            i_5 = i_5 + 1;
        }
        return out_4;
    }

    static void Example() {
        String message = "This is a test.";
        int[] msgBytes = bytesOfStr(message);
        System.out.println("\"" + message + "\" as bytes: " + String.valueOf(bytesToDec(msgBytes)));
        System.out.println("    original bits: " + String.valueOf(bytesToBits(msgBytes)));
        Writer bw = NewWriter("MSB");
        int i_6 = 0;
        while (i_6 < msgBytes.length) {
            bw = WriteBits(bw, msgBytes[i_6], 7);
            i_6 = i_6 + 1;
        }
        bw = CloseWriter(bw);
        System.out.println("Written bitstream: " + String.valueOf(bytesToBits(bw.data)));
        System.out.println("Written bytes: " + String.valueOf(bytesToHex(bw.data)));
        Reader br = NewReader(bw.data, "MSB");
        String result = "";
        while (true) {
            java.util.Map<String,Object> r = ReadBits(br, 7);
            if (((boolean) (r.get("eof")))) {
                break;
            }
            int v_1 = ((Number)(((int) (r.get("val"))))).intValue();
            if (v_1 != 0) {
                result = result + String.valueOf(chr(v_1));
            }
        }
        System.out.println("Read back as \"" + result + "\"");
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            Example();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }

    static int _modPow2(int v, int n) {
        long mask = (1L << n) - 1L;
        return (int)(((long)v) & mask);
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
