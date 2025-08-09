(ns main (:refer-clojure :exclude [extended_euclid chinese_remainder_theorem invert_modulo chinese_remainder_theorem2]))

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

(declare extended_euclid chinese_remainder_theorem invert_modulo chinese_remainder_theorem2)

(def ^:dynamic chinese_remainder_theorem2_m nil)

(def ^:dynamic chinese_remainder_theorem2_n nil)

(def ^:dynamic chinese_remainder_theorem2_x nil)

(def ^:dynamic chinese_remainder_theorem2_y nil)

(def ^:dynamic chinese_remainder_theorem_m nil)

(def ^:dynamic chinese_remainder_theorem_n nil)

(def ^:dynamic chinese_remainder_theorem_res nil)

(def ^:dynamic chinese_remainder_theorem_x nil)

(def ^:dynamic chinese_remainder_theorem_y nil)

(def ^:dynamic extended_euclid_k nil)

(def ^:dynamic extended_euclid_res nil)

(def ^:dynamic invert_modulo_b nil)

(def ^:dynamic invert_modulo_res nil)

(defn extended_euclid [extended_euclid_a extended_euclid_b]
  (binding [extended_euclid_k nil extended_euclid_res nil] (try (do (when (= extended_euclid_b 0) (throw (ex-info "return" {:v {:x 1 :y 0}}))) (set! extended_euclid_res (extended_euclid extended_euclid_b (mod extended_euclid_a extended_euclid_b))) (set! extended_euclid_k (quot extended_euclid_a extended_euclid_b)) (throw (ex-info "return" {:v {:x (:y extended_euclid_res) :y (- (:x extended_euclid_res) (* extended_euclid_k (:y extended_euclid_res)))}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn chinese_remainder_theorem [chinese_remainder_theorem_n1 chinese_remainder_theorem_r1 chinese_remainder_theorem_n2 chinese_remainder_theorem_r2]
  (binding [chinese_remainder_theorem_m nil chinese_remainder_theorem_n nil chinese_remainder_theorem_res nil chinese_remainder_theorem_x nil chinese_remainder_theorem_y nil] (try (do (set! chinese_remainder_theorem_res (extended_euclid chinese_remainder_theorem_n1 chinese_remainder_theorem_n2)) (set! chinese_remainder_theorem_x (:x chinese_remainder_theorem_res)) (set! chinese_remainder_theorem_y (:y chinese_remainder_theorem_res)) (set! chinese_remainder_theorem_m (* chinese_remainder_theorem_n1 chinese_remainder_theorem_n2)) (set! chinese_remainder_theorem_n (+ (* (* chinese_remainder_theorem_r2 chinese_remainder_theorem_x) chinese_remainder_theorem_n1) (* (* chinese_remainder_theorem_r1 chinese_remainder_theorem_y) chinese_remainder_theorem_n2))) (throw (ex-info "return" {:v (mod (+ (mod chinese_remainder_theorem_n chinese_remainder_theorem_m) chinese_remainder_theorem_m) chinese_remainder_theorem_m)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn invert_modulo [invert_modulo_a invert_modulo_n]
  (binding [invert_modulo_b nil invert_modulo_res nil] (try (do (set! invert_modulo_res (extended_euclid invert_modulo_a invert_modulo_n)) (set! invert_modulo_b (:x invert_modulo_res)) (when (< invert_modulo_b 0) (set! invert_modulo_b (mod (+ (mod invert_modulo_b invert_modulo_n) invert_modulo_n) invert_modulo_n))) (throw (ex-info "return" {:v invert_modulo_b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn chinese_remainder_theorem2 [chinese_remainder_theorem2_n1 chinese_remainder_theorem2_r1 chinese_remainder_theorem2_n2 chinese_remainder_theorem2_r2]
  (binding [chinese_remainder_theorem2_m nil chinese_remainder_theorem2_n nil chinese_remainder_theorem2_x nil chinese_remainder_theorem2_y nil] (try (do (set! chinese_remainder_theorem2_x (invert_modulo chinese_remainder_theorem2_n1 chinese_remainder_theorem2_n2)) (set! chinese_remainder_theorem2_y (invert_modulo chinese_remainder_theorem2_n2 chinese_remainder_theorem2_n1)) (set! chinese_remainder_theorem2_m (* chinese_remainder_theorem2_n1 chinese_remainder_theorem2_n2)) (set! chinese_remainder_theorem2_n (+ (* (* chinese_remainder_theorem2_r2 chinese_remainder_theorem2_x) chinese_remainder_theorem2_n1) (* (* chinese_remainder_theorem2_r1 chinese_remainder_theorem2_y) chinese_remainder_theorem2_n2))) (throw (ex-info "return" {:v (mod (+ (mod chinese_remainder_theorem2_n chinese_remainder_theorem2_m) chinese_remainder_theorem2_m) chinese_remainder_theorem2_m)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_e1 (extended_euclid 10 6))

(def ^:dynamic main_e2 (extended_euclid 7 5))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (str (str (:x main_e1)) ",") (str (:y main_e1))))
      (println (str (str (str (:x main_e2)) ",") (str (:y main_e2))))
      (println (str (chinese_remainder_theorem 5 1 7 3)))
      (println (str (chinese_remainder_theorem 6 1 4 3)))
      (println (str (invert_modulo 2 5)))
      (println (str (invert_modulo 8 7)))
      (println (str (chinese_remainder_theorem2 5 1 7 3)))
      (println (str (chinese_remainder_theorem2 6 1 4 3)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
