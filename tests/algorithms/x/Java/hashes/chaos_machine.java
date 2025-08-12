public class Main {
    static double[] K;
    static long t;
    static long size;
    static class Machine {
        double[] buffer;
        double[] params;
        long time;
        Machine(double[] buffer, double[] params, long time) {
            this.buffer = buffer;
            this.params = params;
            this.time = time;
        }
        Machine() {}
        @Override public String toString() {
            return String.format("{'buffer': %s, 'params': %s, 'time': %s}", String.valueOf(buffer), String.valueOf(params), String.valueOf(time));
        }
    }

    static class PullResult {
        long value;
        Machine machine;
        PullResult(long value, Machine machine) {
            this.value = value;
            this.machine = machine;
        }
        PullResult() {}
        @Override public String toString() {
            return String.format("{'value': %s, 'machine': %s}", String.valueOf(value), String.valueOf(machine));
        }
    }

    static Machine machine = null;
    static long i_6 = 0;
    static PullResult res_2;

    static double round_dec(double x, long n) {
        double m10 = 1.0;
        long i_1 = 0L;
        while (i_1 < n) {
            m10 = m10 * 10.0;
            i_1 = i_1 + 1;
        }
        double y_1 = x * m10 + 0.5;
        return (1.0 * ((Number)(y_1)).intValue()) / m10;
    }

    static Machine reset() {
        return new Machine(K, new double[]{0.0, 0.0, 0.0, 0.0, 0.0}, 0);
    }

    static Machine push(Machine m, long seed) {
        double[] buf = ((double[])(m.buffer));
        double[] par_1 = ((double[])(m.params));
        long i_3 = 0L;
        while (i_3 < buf.length) {
            double value_1 = buf[(int)((long)(i_3))];
            double e_1 = (1.0 * seed) / value_1;
            double next_value_1 = buf[(int)((long)(Math.floorMod((i_3 + 1), size)))] + e_1;
            next_value_1 = next_value_1 - (1.0 * ((Number)(next_value_1)).intValue());
            double r_1 = par_1[(int)((long)(i_3))] + e_1;
            r_1 = r_1 - (1.0 * ((Number)(r_1)).intValue());
            r_1 = r_1 + 3.0;
buf[(int)((long)(i_3))] = round_dec(r_1 * next_value_1 * (1.0 - next_value_1), 10L);
par_1[(int)((long)(i_3))] = r_1;
            i_3 = i_3 + 1;
        }
        return new Machine(buf, par_1, m.time + 1);
    }

    static long xor(long a, long b) {
        long aa = a;
        long bb_1 = b;
        long res_1 = 0L;
        long bit_1 = 1L;
        while (aa > 0 || bb_1 > 0) {
            long abit_1 = Math.floorMod(aa, 2);
            long bbit_1 = Math.floorMod(bb_1, 2);
            if (abit_1 != bbit_1) {
                res_1 = res_1 + bit_1;
            }
            aa = Math.floorDiv(aa, 2);
            bb_1 = Math.floorDiv(bb_1, 2);
            bit_1 = bit_1 * 2;
        }
        return res_1;
    }

    static long xorshift(long x, long y) {
        long xv = x;
        long yv_1 = y;
        xv = xor(xv, Math.floorDiv(yv_1, 8192));
        yv_1 = xor(yv_1, xv * 131072);
        xv = xor(xv, Math.floorDiv(yv_1, 32));
        return xv;
    }

    static PullResult pull(Machine m) {
        double[] buf_1 = ((double[])(m.buffer));
        double[] par_3 = ((double[])(m.params));
        long key_1 = Math.floorMod(m.time, size);
        long i_5 = 0L;
        while (i_5 < t) {
            double r_3 = par_3[(int)((long)(key_1))];
            double value_3 = buf_1[(int)((long)(key_1))];
buf_1[(int)((long)(key_1))] = round_dec(r_3 * value_3 * (1.0 - value_3), 10L);
            double new_r_1 = (1.0 * m.time) * 0.01 + r_3 * 1.01;
            new_r_1 = new_r_1 - (1.0 * ((Number)(new_r_1)).intValue());
par_3[(int)((long)(key_1))] = new_r_1 + 3.0;
            i_5 = i_5 + 1;
        }
        long x_1 = ((Number)(buf_1[(int)((long)(Math.floorMod((key_1 + 2), size)))] * 10000000000.0)).intValue();
        long y_3 = ((Number)(buf_1[(int)((long)(Math.floorMod((key_1 + size - 2), size)))] * 10000000000.0)).intValue();
        Machine new_machine_1 = new Machine(buf_1, par_3, m.time + 1);
        long value_5 = Math.floorMod(xorshift(x_1, y_3), 4294967295L);
        return new PullResult(value_5, new_machine_1);
    }
    public static void main(String[] args) {
        K = ((double[])(new double[]{0.33, 0.44, 0.55, 0.44, 0.33}));
        t = 3L;
        size = 5L;
        machine = reset();
        i_6 = 0;
        while (i_6 < 100) {
            machine = push(machine, i_6);
            i_6 = i_6 + 1;
        }
        res_2 = pull(machine);
        System.out.println(res_2.value);
        System.out.println(java.util.Arrays.toString(res_2.machine.buffer));
        System.out.println(java.util.Arrays.toString(res_2.machine.params));
    }
}
