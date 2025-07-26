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
            int b = Math.floorMod(rshift(w.bits, 24), 256);
w.data = java.util.stream.IntStream.concat(java.util.Arrays.stream(w.data), java.util.stream.IntStream.of(b)).toArray();
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
            int b = r.data[r.idx];
r.idx = r.idx + 1;
r.bits = r.bits + lshift(b, r.nbits);
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
            int b = r.data[r.idx];
r.idx = r.idx + 1;
r.bits = r.bits + lshift(b, 24 - r.nbits);
r.nbits = r.nbits + 8;
        }
        int out = rshift(r.bits, 32 - width);
r.bits = _modPow2((r.bits * pow2(width)), 32);
r.nbits = r.nbits - width;
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("val", out), java.util.Map.entry("eof", false)));
    }

    static java.util.Map<String,Object> ReadBits(Reader r, int width) {
        if ((r.order.equals("LSB"))) {
            return readBitsLSB(r, width);
        }
        return readBitsMSB(r, width);
    }

    static String toBinary(int n, int bits) {
        String b = "";
        int val = n;
        int i = 0;
        while (i < bits) {
            b = String.valueOf(Math.floorMod(val, 2)) + b;
            val = val / 2;
            i = i + 1;
        }
        return b;
    }

    static String bytesToBits(int[] bs) {
        String out = "[";
        int i = 0;
        while (i < bs.length) {
            out = out + String.valueOf(toBinary(bs[i], 8));
            if (i + 1 < bs.length) {
                out = out + " ";
            }
            i = i + 1;
        }
        out = out + "]";
        return out;
    }

    static String bytesToHex(int[] bs) {
        String digits = "0123456789ABCDEF";
        String out = "";
        int i = 0;
        while (i < bs.length) {
            int b = bs[i];
            int hi = b / 16;
            int lo = Math.floorMod(b, 16);
            out = out + digits.substring(hi, hi + 1) + digits.substring(lo, lo + 1);
            if (i + 1 < bs.length) {
                out = out + " ";
            }
            i = i + 1;
        }
        return out;
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
        if (((ch.compareTo("0") >= 0) && ch.compareTo("9") <= 0)) {
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
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower = "abcdefghijklmnopqrstuvwxyz";
        if (n >= 65 && n < 91) {
            return upper.substring(n - 65, n - 64);
        }
        if (n >= 97 && n < 123) {
            return lower.substring(n - 97, n - 96);
        }
        if (n >= 48 && n < 58) {
            String digits = "0123456789";
            return digits.substring(n - 48, n - 47);
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
        int i = 0;
        while (i < s.length()) {
            bs = java.util.stream.IntStream.concat(java.util.Arrays.stream(bs), java.util.stream.IntStream.of(ord(s.substring(i, i + 1)))).toArray();
            i = i + 1;
        }
        return bs;
    }

    static String bytesToDec(int[] bs) {
        String out = "";
        int i = 0;
        while (i < bs.length) {
            out = out + String.valueOf(bs[i]);
            if (i + 1 < bs.length) {
                out = out + " ";
            }
            i = i + 1;
        }
        return out;
    }

    static void Example() {
        String message = "This is a test.";
        int[] msgBytes = bytesOfStr(message);
        System.out.println("\"" + message + "\" as bytes: " + String.valueOf(bytesToDec(msgBytes)));
        System.out.println("    original bits: " + String.valueOf(bytesToBits(msgBytes)));
        Writer bw = NewWriter("MSB");
        int i = 0;
        while (i < msgBytes.length) {
            bw = WriteBits(bw, msgBytes[i], 7);
            i = i + 1;
        }
        bw = CloseWriter(bw);
        System.out.println("Written bitstream: " + String.valueOf(bytesToBits(bw.data)));
        System.out.println("Written bytes: " + String.valueOf(bytesToHex(bw.data)));
        Reader br = NewReader(bw.data, "MSB");
        String result = "";
        while (true) {
            java.util.Map<String,Object> r = ReadBits(br, 7);
            if (((boolean)r.getOrDefault("eof", false))) {
                break;
            }
            int v = (int)(((int)r.getOrDefault("val", 0)));
            if (v != 0) {
                result = result + String.valueOf(chr(v));
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
}
