(ns main (:refer-clojure :exclude [round_dec reset push xor xorshift pull]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (Integer/parseInt (str s)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare round_dec reset push xor xorshift pull)

(def ^:dynamic main_i nil)

(def ^:dynamic main_machine nil)

(def ^:dynamic pull_buf nil)

(def ^:dynamic pull_i nil)

(def ^:dynamic pull_key nil)

(def ^:dynamic pull_new_machine nil)

(def ^:dynamic pull_new_r nil)

(def ^:dynamic pull_par nil)

(def ^:dynamic pull_r nil)

(def ^:dynamic pull_value nil)

(def ^:dynamic pull_x nil)

(def ^:dynamic pull_y nil)

(def ^:dynamic push_buf nil)

(def ^:dynamic push_e nil)

(def ^:dynamic push_i nil)

(def ^:dynamic push_next_value nil)

(def ^:dynamic push_par nil)

(def ^:dynamic push_r nil)

(def ^:dynamic push_value nil)

(def ^:dynamic round_dec_i nil)

(def ^:dynamic round_dec_m10 nil)

(def ^:dynamic round_dec_y nil)

(def ^:dynamic xor_aa nil)

(def ^:dynamic xor_abit nil)

(def ^:dynamic xor_bb nil)

(def ^:dynamic xor_bbit nil)

(def ^:dynamic xor_bit nil)

(def ^:dynamic xor_res nil)

(def ^:dynamic xorshift_xv nil)

(def ^:dynamic xorshift_yv nil)

(def ^:dynamic main_K [0.33 0.44 0.55 0.44 0.33])

(def ^:dynamic main_t 3)

(def ^:dynamic main_size 5)

(defn round_dec [round_dec_x round_dec_n]
  (binding [round_dec_i nil round_dec_m10 nil round_dec_y nil] (try (do (set! round_dec_m10 1.0) (set! round_dec_i 0) (while (< round_dec_i round_dec_n) (do (set! round_dec_m10 (* round_dec_m10 10.0)) (set! round_dec_i (+ round_dec_i 1)))) (set! round_dec_y (+ (* round_dec_x round_dec_m10) 0.5)) (throw (ex-info "return" {:v (quot (* 1.0 (Integer/parseInt round_dec_y)) round_dec_m10)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reset []
  (try (throw (ex-info "return" {:v {:buffer main_K :params [0.0 0.0 0.0 0.0 0.0] :time 0}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn push [push_m push_seed]
  (binding [push_buf nil push_e nil push_i nil push_next_value nil push_par nil push_r nil push_value nil] (try (do (set! push_buf (:buffer push_m)) (set! push_par (:params push_m)) (set! push_i 0) (while (< push_i (count push_buf)) (do (set! push_value (get push_buf push_i)) (set! push_e (quot (* 1.0 push_seed) push_value)) (set! push_next_value (+ (get push_buf (mod (+ push_i 1) main_size)) push_e)) (set! push_next_value (- push_next_value (* 1.0 (Integer/parseInt push_next_value)))) (set! push_r (+ (get push_par push_i) push_e)) (set! push_r (- push_r (* 1.0 (Integer/parseInt push_r)))) (set! push_r (+ push_r 3.0)) (set! push_buf (assoc push_buf push_i (round_dec (* (* push_r push_next_value) (- 1.0 push_next_value)) 10))) (set! push_par (assoc push_par push_i push_r)) (set! push_i (+ push_i 1)))) (throw (ex-info "return" {:v {:buffer push_buf :params push_par :time (+ (:time push_m) 1)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn xor [xor_a xor_b]
  (binding [xor_aa nil xor_abit nil xor_bb nil xor_bbit nil xor_bit nil xor_res nil] (try (do (set! xor_aa xor_a) (set! xor_bb xor_b) (set! xor_res 0) (set! xor_bit 1) (while (or (> xor_aa 0) (> xor_bb 0)) (do (set! xor_abit (mod xor_aa 2)) (set! xor_bbit (mod xor_bb 2)) (when (not= xor_abit xor_bbit) (set! xor_res (+ xor_res xor_bit))) (set! xor_aa (quot xor_aa 2)) (set! xor_bb (quot xor_bb 2)) (set! xor_bit (* xor_bit 2)))) (throw (ex-info "return" {:v xor_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn xorshift [xorshift_x xorshift_y]
  (binding [xorshift_xv nil xorshift_yv nil] (try (do (set! xorshift_xv xorshift_x) (set! xorshift_yv xorshift_y) (set! xorshift_xv (xor xorshift_xv (quot xorshift_yv 8192))) (set! xorshift_yv (xor xorshift_yv (* xorshift_xv 131072))) (set! xorshift_xv (xor xorshift_xv (quot xorshift_yv 32))) (throw (ex-info "return" {:v xorshift_xv}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pull [pull_m]
  (binding [pull_buf nil pull_i nil pull_key nil pull_new_machine nil pull_new_r nil pull_par nil pull_r nil pull_value nil pull_x nil pull_y nil] (try (do (set! pull_buf (:buffer pull_m)) (set! pull_par (:params pull_m)) (set! pull_key (mod (:time pull_m) main_size)) (set! pull_i 0) (while (< pull_i main_t) (do (set! pull_r (get pull_par pull_key)) (set! pull_value (get pull_buf pull_key)) (set! pull_buf (assoc pull_buf pull_key (round_dec (* (* pull_r pull_value) (- 1.0 pull_value)) 10))) (set! pull_new_r (+ (* (* 1.0 (:time pull_m)) 0.01) (* pull_r 1.01))) (set! pull_new_r (- pull_new_r (* 1.0 (Integer/parseInt pull_new_r)))) (set! pull_par (assoc pull_par pull_key (+ pull_new_r 3.0))) (set! pull_i (+ pull_i 1)))) (set! pull_x (Integer/parseInt (* (get pull_buf (mod (+ pull_key 2) main_size)) 10000000000.0))) (set! pull_y (Integer/parseInt (* (get pull_buf (mod (- (+ pull_key main_size) 2) main_size)) 10000000000.0))) (set! pull_new_machine {:buffer pull_buf :params pull_par :time (+ (:time pull_m) 1)}) (set! pull_value (mod (xorshift pull_x pull_y) 4294967295)) (throw (ex-info "return" {:v {:machine pull_new_machine :value pull_value}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_machine (reset))

(def ^:dynamic main_i 0)

(def ^:dynamic main_res (pull main_machine))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_i 100) (do (def main_machine (push main_machine main_i)) (def main_i (+ main_i 1))))
      (println (:value main_res))
      (println (:buffer (:machine main_res)))
      (println (:params (:machine main_res)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
