(ns main (:refer-clojure :exclude [key viterbi join_words]))

(require 'clojure.set)

(defrecord EmitP [Healthy Fever])

(defrecord TransP [Healthy Fever])

(defrecord StartP [Healthy Fever])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(def ^:dynamic join_words_i nil)

(def ^:dynamic join_words_res nil)

(def ^:dynamic viterbi_first_obs nil)

(def ^:dynamic viterbi_i nil)

(def ^:dynamic viterbi_idx nil)

(def ^:dynamic viterbi_j nil)

(def ^:dynamic viterbi_k nil)

(def ^:dynamic viterbi_last_index nil)

(def ^:dynamic viterbi_last_obs nil)

(def ^:dynamic viterbi_last_state nil)

(def ^:dynamic viterbi_m nil)

(def ^:dynamic viterbi_max_final nil)

(def ^:dynamic viterbi_max_prob nil)

(def ^:dynamic viterbi_n nil)

(def ^:dynamic viterbi_obs nil)

(def ^:dynamic viterbi_obs0 nil)

(def ^:dynamic viterbi_path nil)

(def ^:dynamic viterbi_prev nil)

(def ^:dynamic viterbi_prev_state nil)

(def ^:dynamic viterbi_prob nil)

(def ^:dynamic viterbi_prob_prev nil)

(def ^:dynamic viterbi_probs nil)

(def ^:dynamic viterbi_ptrs nil)

(def ^:dynamic viterbi_state nil)

(def ^:dynamic viterbi_state0 nil)

(def ^:dynamic viterbi_t nil)

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare key viterbi join_words)

(defn key [key_state key_obs]
  (try (throw (ex-info "return" {:v (str (str key_state "|") key_obs)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn viterbi [viterbi_observations viterbi_states viterbi_start_p viterbi_trans_p viterbi_emit_p]
  (binding [viterbi_first_obs nil viterbi_i nil viterbi_idx nil viterbi_j nil viterbi_k nil viterbi_last_index nil viterbi_last_obs nil viterbi_last_state nil viterbi_m nil viterbi_max_final nil viterbi_max_prob nil viterbi_n nil viterbi_obs nil viterbi_obs0 nil viterbi_path nil viterbi_prev nil viterbi_prev_state nil viterbi_prob nil viterbi_prob_prev nil viterbi_probs nil viterbi_ptrs nil viterbi_state nil viterbi_state0 nil viterbi_t nil] (try (do (when (or (= (count viterbi_observations) 0) (= (count viterbi_states) 0)) (throw (Exception. "empty parameters"))) (set! viterbi_probs {}) (set! viterbi_ptrs {}) (set! viterbi_first_obs (nth viterbi_observations 0)) (set! viterbi_i 0) (while (< viterbi_i (count viterbi_states)) (do (set! viterbi_state (nth viterbi_states viterbi_i)) (set! viterbi_probs (assoc viterbi_probs (key viterbi_state viterbi_first_obs) (* (get viterbi_start_p viterbi_state) (get (get viterbi_emit_p viterbi_state) viterbi_first_obs)))) (set! viterbi_ptrs (assoc viterbi_ptrs (key viterbi_state viterbi_first_obs) "")) (set! viterbi_i (+ viterbi_i 1)))) (set! viterbi_t 1) (while (< viterbi_t (count viterbi_observations)) (do (set! viterbi_obs (nth viterbi_observations viterbi_t)) (set! viterbi_j 0) (while (< viterbi_j (count viterbi_states)) (do (set! viterbi_state (nth viterbi_states viterbi_j)) (set! viterbi_max_prob (- 1.0)) (set! viterbi_prev_state "") (set! viterbi_k 0) (while (< viterbi_k (count viterbi_states)) (do (set! viterbi_state0 (nth viterbi_states viterbi_k)) (set! viterbi_obs0 (nth viterbi_observations (- viterbi_t 1))) (set! viterbi_prob_prev (get viterbi_probs (key viterbi_state0 viterbi_obs0))) (set! viterbi_prob (* (* viterbi_prob_prev (get (get viterbi_trans_p viterbi_state0) viterbi_state)) (get (get viterbi_emit_p viterbi_state) viterbi_obs))) (when (> viterbi_prob viterbi_max_prob) (do (set! viterbi_max_prob viterbi_prob) (set! viterbi_prev_state viterbi_state0))) (set! viterbi_k (+ viterbi_k 1)))) (set! viterbi_probs (assoc viterbi_probs (key viterbi_state viterbi_obs) viterbi_max_prob)) (set! viterbi_ptrs (assoc viterbi_ptrs (key viterbi_state viterbi_obs) viterbi_prev_state)) (set! viterbi_j (+ viterbi_j 1)))) (set! viterbi_t (+ viterbi_t 1)))) (set! viterbi_path []) (set! viterbi_n 0) (while (< viterbi_n (count viterbi_observations)) (do (set! viterbi_path (conj viterbi_path "")) (set! viterbi_n (+ viterbi_n 1)))) (set! viterbi_last_obs (nth viterbi_observations (- (count viterbi_observations) 1))) (set! viterbi_max_final (- 1.0)) (set! viterbi_last_state "") (set! viterbi_m 0) (while (< viterbi_m (count viterbi_states)) (do (set! viterbi_state (nth viterbi_states viterbi_m)) (set! viterbi_prob (get viterbi_probs (key viterbi_state viterbi_last_obs))) (when (> viterbi_prob viterbi_max_final) (do (set! viterbi_max_final viterbi_prob) (set! viterbi_last_state viterbi_state))) (set! viterbi_m (+ viterbi_m 1)))) (set! viterbi_last_index (- (count viterbi_observations) 1)) (set! viterbi_path (assoc viterbi_path viterbi_last_index viterbi_last_state)) (set! viterbi_idx viterbi_last_index) (while (> viterbi_idx 0) (do (set! viterbi_obs (nth viterbi_observations viterbi_idx)) (set! viterbi_prev (get viterbi_ptrs (key (nth viterbi_path viterbi_idx) viterbi_obs))) (set! viterbi_path (assoc viterbi_path (- viterbi_idx 1) viterbi_prev)) (set! viterbi_idx (- viterbi_idx 1)))) (throw (ex-info "return" {:v viterbi_path}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn join_words [join_words_words]
  (binding [join_words_i nil join_words_res nil] (try (do (set! join_words_res "") (set! join_words_i 0) (while (< join_words_i (count join_words_words)) (do (when (> join_words_i 0) (set! join_words_res (str join_words_res " "))) (set! join_words_res (str join_words_res (nth join_words_words join_words_i))) (set! join_words_i (+ join_words_i 1)))) (throw (ex-info "return" {:v join_words_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_observations ["normal" "cold" "dizzy"])

(def ^:dynamic main_states ["Healthy" "Fever"])

(def ^:dynamic main_start_p {"Fever" 0.4 "Healthy" 0.6})

(def ^:dynamic main_trans_p {"Fever" {"Fever" 0.6 "Healthy" 0.4} "Healthy" {"Fever" 0.3 "Healthy" 0.7}})

(def ^:dynamic main_emit_p {"Fever" {"cold" 0.3 "dizzy" 0.6 "normal" 0.1} "Healthy" {"cold" 0.4 "dizzy" 0.1 "normal" 0.5}})

(def ^:dynamic main_result (viterbi main_observations main_states main_start_p main_trans_p main_emit_p))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (join_words main_result))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
