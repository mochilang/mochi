public class Main {
    static String[] observations = ((String[])(new String[]{"normal", "cold", "dizzy"}));
    static String[] states = ((String[])(new String[]{"Healthy", "Fever"}));
    static java.util.Map<String,Double> start_p = null;
    static java.util.Map<String,java.util.Map<String,Double>> trans_p = null;
    static java.util.Map<String,java.util.Map<String,Double>> emit_p = null;
    static String[] result;

    static String key(String state, String obs) {
        return state + "|" + obs;
    }

    static String[] viterbi(String[] observations, String[] states, java.util.Map<String,Double> start_p, java.util.Map<String,java.util.Map<String,Double>> trans_p, java.util.Map<String,java.util.Map<String,Double>> emit_p) {
        if (new java.math.BigInteger(String.valueOf(observations.length)).compareTo(java.math.BigInteger.valueOf(0)) == 0 || new java.math.BigInteger(String.valueOf(states.length)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            throw new RuntimeException(String.valueOf("empty parameters"));
        }
        java.util.Map<String,Double> probs_1 = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>()));
        java.util.Map<String,String> ptrs_1 = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>()));
        String first_obs_1 = observations[_idx((observations).length, 0L)];
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(states.length))) < 0) {
            String state_1 = states[_idx((states).length, ((java.math.BigInteger)(i_1)).longValue())];
probs_1.put(key(state_1, first_obs_1), (double)((double)(((double)(start_p).getOrDefault(state_1, 0.0))) * (double)(((double)(((java.util.Map<String,Double>)(emit_p).get(state_1))).getOrDefault(first_obs_1, 0.0)))));
ptrs_1.put(key(state_1, first_obs_1), "");
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger t_1 = java.math.BigInteger.valueOf(1);
        while (t_1.compareTo(new java.math.BigInteger(String.valueOf(observations.length))) < 0) {
            String obs_1 = observations[_idx((observations).length, ((java.math.BigInteger)(t_1)).longValue())];
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(new java.math.BigInteger(String.valueOf(states.length))) < 0) {
                String state_3 = states[_idx((states).length, ((java.math.BigInteger)(j_1)).longValue())];
                double max_prob_1 = (double)(-1.0);
                String prev_state_1 = "";
                java.math.BigInteger k_1 = java.math.BigInteger.valueOf(0);
                while (k_1.compareTo(new java.math.BigInteger(String.valueOf(states.length))) < 0) {
                    String state0_1 = states[_idx((states).length, ((java.math.BigInteger)(k_1)).longValue())];
                    String obs0_1 = observations[_idx((observations).length, ((java.math.BigInteger)(t_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())];
                    double prob_prev_1 = (double)(((double)(probs_1).getOrDefault(key(state0_1, obs0_1), 0.0)));
                    double prob_1 = (double)((double)((double)(prob_prev_1) * (double)(((double)(((java.util.Map<String,Double>)(trans_p).get(state0_1))).getOrDefault(state_3, 0.0)))) * (double)(((double)(((java.util.Map<String,Double>)(emit_p).get(state_3))).getOrDefault(obs_1, 0.0))));
                    if ((double)(prob_1) > (double)(max_prob_1)) {
                        max_prob_1 = (double)(prob_1);
                        prev_state_1 = state0_1;
                    }
                    k_1 = new java.math.BigInteger(String.valueOf(k_1.add(java.math.BigInteger.valueOf(1))));
                }
probs_1.put(key(state_3, obs_1), (double)(max_prob_1));
ptrs_1.put(key(state_3, obs_1), prev_state_1);
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            t_1 = new java.math.BigInteger(String.valueOf(t_1.add(java.math.BigInteger.valueOf(1))));
        }
        String[] path_1 = ((String[])(new String[]{}));
        java.math.BigInteger n_1 = java.math.BigInteger.valueOf(0);
        while (n_1.compareTo(new java.math.BigInteger(String.valueOf(observations.length))) < 0) {
            path_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(path_1), java.util.stream.Stream.of("")).toArray(String[]::new)));
            n_1 = new java.math.BigInteger(String.valueOf(n_1.add(java.math.BigInteger.valueOf(1))));
        }
        String last_obs_1 = observations[_idx((observations).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(observations.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue())];
        double max_final_1 = (double)(-1.0);
        String last_state_1 = "";
        java.math.BigInteger m_1 = java.math.BigInteger.valueOf(0);
        while (m_1.compareTo(new java.math.BigInteger(String.valueOf(states.length))) < 0) {
            String state_5 = states[_idx((states).length, ((java.math.BigInteger)(m_1)).longValue())];
            double prob_3 = (double)(((double)(probs_1).getOrDefault(key(state_5, last_obs_1), 0.0)));
            if ((double)(prob_3) > (double)(max_final_1)) {
                max_final_1 = (double)(prob_3);
                last_state_1 = state_5;
            }
            m_1 = new java.math.BigInteger(String.valueOf(m_1.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger last_index_1 = new java.math.BigInteger(String.valueOf(new java.math.BigInteger(String.valueOf(observations.length)).subtract(java.math.BigInteger.valueOf(1))));
path_1[(int)(((java.math.BigInteger)(last_index_1)).longValue())] = last_state_1;
        java.math.BigInteger idx_1 = new java.math.BigInteger(String.valueOf(last_index_1));
        while (idx_1.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            String obs_3 = observations[_idx((observations).length, ((java.math.BigInteger)(idx_1)).longValue())];
            String prev_1 = ((String)(ptrs_1).get(key(path_1[_idx((path_1).length, ((java.math.BigInteger)(idx_1)).longValue())], obs_3)));
path_1[(int)(((java.math.BigInteger)(idx_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())] = prev_1;
            idx_1 = new java.math.BigInteger(String.valueOf(idx_1.subtract(java.math.BigInteger.valueOf(1))));
        }
        return ((String[])(path_1));
    }

    static String join_words(String[] words) {
        String res = "";
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(words.length))) < 0) {
            if (i_3.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
                res = res + " ";
            }
            res = res + words[_idx((words).length, ((java.math.BigInteger)(i_3)).longValue())];
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return res;
    }
    public static void main(String[] args) {
        start_p = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("Healthy", (double)(0.6)); put("Fever", (double)(0.4)); }}));
        trans_p = ((java.util.Map<String,java.util.Map<String,Double>>)(new java.util.LinkedHashMap<String, java.util.Map<String,Double>>() {{ put("Healthy", ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("Healthy", (double)(0.7)); put("Fever", (double)(0.3)); }}))); put("Fever", ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("Healthy", (double)(0.4)); put("Fever", (double)(0.6)); }}))); }}));
        emit_p = ((java.util.Map<String,java.util.Map<String,Double>>)(new java.util.LinkedHashMap<String, java.util.Map<String,Double>>() {{ put("Healthy", ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("normal", (double)(0.5)); put("cold", (double)(0.4)); put("dizzy", (double)(0.1)); }}))); put("Fever", ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("normal", (double)(0.1)); put("cold", (double)(0.3)); put("dizzy", (double)(0.6)); }}))); }}));
        result = ((String[])(viterbi(((String[])(observations)), ((String[])(states)), start_p, trans_p, emit_p)));
        System.out.println(join_words(((String[])(result))));
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
