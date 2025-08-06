public class Main {
    static String[] observations = new String[0];
    static String[] states = new String[0];
    static java.util.Map<String,Double> start_p = null;
    static java.util.Map<String,java.util.Map<String,Double>> trans_p = null;
    static java.util.Map<String,java.util.Map<String,Double>> emit_p = null;
    static String[] result;

    static String key(String state, String obs) {
        return state + "|" + obs;
    }

    static String[] viterbi(String[] observations, String[] states, java.util.Map<String,Double> start_p, java.util.Map<String,java.util.Map<String,Double>> trans_p, java.util.Map<String,java.util.Map<String,Double>> emit_p) {
        if (observations.length == 0 || states.length == 0) {
            throw new RuntimeException(String.valueOf("empty parameters"));
        }
        java.util.Map<String,Double> probs = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>()));
        java.util.Map<String,String> ptrs = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>()));
        String first_obs = observations[0];
        int i = 0;
        while (i < states.length) {
            String state = states[i];
probs.put(key(state, first_obs), (double)(((double)(start_p).getOrDefault(state, 0.0))) * (double)(((double)(((java.util.Map<String,Double>)(emit_p).get(state))).getOrDefault(first_obs, 0.0))));
ptrs.put(key(state, first_obs), "");
            i = i + 1;
        }
        int t = 1;
        while (t < observations.length) {
            String obs = observations[t];
            int j = 0;
            while (j < states.length) {
                String state_1 = states[j];
                double max_prob = -1.0;
                String prev_state = "";
                int k = 0;
                while (k < states.length) {
                    String state0 = states[k];
                    String obs0 = observations[t - 1];
                    double prob_prev = (double)(((double)(probs).getOrDefault(key(state0, obs0), 0.0)));
                    double prob = prob_prev * (double)(((double)(((java.util.Map<String,Double>)(trans_p).get(state0))).getOrDefault(state_1, 0.0))) * (double)(((double)(((java.util.Map<String,Double>)(emit_p).get(state_1))).getOrDefault(obs, 0.0)));
                    if (prob > max_prob) {
                        max_prob = prob;
                        prev_state = state0;
                    }
                    k = k + 1;
                }
probs.put(key(state_1, obs), max_prob);
ptrs.put(key(state_1, obs), prev_state);
                j = j + 1;
            }
            t = t + 1;
        }
        String[] path = ((String[])(new String[]{}));
        int n = 0;
        while (n < observations.length) {
            path = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(path), java.util.stream.Stream.of("")).toArray(String[]::new)));
            n = n + 1;
        }
        String last_obs = observations[observations.length - 1];
        double max_final = -1.0;
        String last_state = "";
        int m = 0;
        while (m < states.length) {
            String state_2 = states[m];
            double prob_1 = (double)(((double)(probs).getOrDefault(key(state_2, last_obs), 0.0)));
            if (prob_1 > max_final) {
                max_final = prob_1;
                last_state = state_2;
            }
            m = m + 1;
        }
        int last_index = observations.length - 1;
path[last_index] = last_state;
        int idx = last_index;
        while (idx > 0) {
            String obs_1 = observations[idx];
            String prev = ((String)(ptrs).get(key(path[idx], obs_1)));
path[idx - 1] = prev;
            idx = idx - 1;
        }
        return path;
    }

    static String join_words(String[] words) {
        String res = "";
        int i_1 = 0;
        while (i_1 < words.length) {
            if (i_1 > 0) {
                res = res + " ";
            }
            res = res + words[i_1];
            i_1 = i_1 + 1;
        }
        return res;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            observations = ((String[])(new String[]{"normal", "cold", "dizzy"}));
            states = ((String[])(new String[]{"Healthy", "Fever"}));
            start_p = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("Healthy", 0.6), java.util.Map.entry("Fever", 0.4)))));
            trans_p = ((java.util.Map<String,java.util.Map<String,Double>>)(new java.util.LinkedHashMap<String, java.util.Map<String,Double>>(java.util.Map.ofEntries(java.util.Map.entry("Healthy", ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("Healthy", 0.7), java.util.Map.entry("Fever", 0.3)))))), java.util.Map.entry("Fever", ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("Healthy", 0.4), java.util.Map.entry("Fever", 0.6))))))))));
            emit_p = ((java.util.Map<String,java.util.Map<String,Double>>)(new java.util.LinkedHashMap<String, java.util.Map<String,Double>>(java.util.Map.ofEntries(java.util.Map.entry("Healthy", ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("normal", 0.5), java.util.Map.entry("cold", 0.4), java.util.Map.entry("dizzy", 0.1)))))), java.util.Map.entry("Fever", ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("normal", 0.1), java.util.Map.entry("cold", 0.3), java.util.Map.entry("dizzy", 0.6))))))))));
            result = ((String[])(viterbi(((String[])(observations)), ((String[])(states)), start_p, trans_p, emit_p)));
            System.out.println(join_words(((String[])(result))));
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
}
