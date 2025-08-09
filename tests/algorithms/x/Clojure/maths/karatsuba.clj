(ns main (:refer-clojure :exclude [int_pow karatsuba main]))

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

(declare int_pow karatsuba main)

(def ^:dynamic int_pow_i nil)

(def ^:dynamic int_pow_result nil)

(def ^:dynamic karatsuba_a1 nil)

(def ^:dynamic karatsuba_a2 nil)

(def ^:dynamic karatsuba_b1 nil)

(def ^:dynamic karatsuba_b2 nil)

(def ^:dynamic karatsuba_lb nil)

(def ^:dynamic karatsuba_m1 nil)

(def ^:dynamic karatsuba_m2 nil)

(def ^:dynamic karatsuba_power nil)

(def ^:dynamic karatsuba_result nil)

(def ^:dynamic karatsuba_x nil)

(def ^:dynamic karatsuba_y nil)

(def ^:dynamic karatsuba_z nil)

(defn int_pow [int_pow_base int_pow_exp]
  (binding [int_pow_i nil int_pow_result nil] (try (do (set! int_pow_result 1) (set! int_pow_i 0) (while (< int_pow_i int_pow_exp) (do (set! int_pow_result (* int_pow_result int_pow_base)) (set! int_pow_i (+ int_pow_i 1)))) (throw (ex-info "return" {:v int_pow_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn karatsuba [karatsuba_a karatsuba_b]
  (binding [karatsuba_a1 nil karatsuba_a2 nil karatsuba_b1 nil karatsuba_b2 nil karatsuba_lb nil karatsuba_m1 nil karatsuba_m2 nil karatsuba_power nil karatsuba_result nil karatsuba_x nil karatsuba_y nil karatsuba_z nil] (try (do (when (or (= (count (str karatsuba_a)) 1) (= (count (str karatsuba_b)) 1)) (throw (ex-info "return" {:v (* karatsuba_a karatsuba_b)}))) (set! karatsuba_m1 (count (str karatsuba_a))) (set! karatsuba_lb (count (str karatsuba_b))) (when (> karatsuba_lb karatsuba_m1) (set! karatsuba_m1 karatsuba_lb)) (set! karatsuba_m2 (quot karatsuba_m1 2)) (set! karatsuba_power (int_pow 10 karatsuba_m2)) (set! karatsuba_a1 (quot karatsuba_a karatsuba_power)) (set! karatsuba_a2 (mod karatsuba_a karatsuba_power)) (set! karatsuba_b1 (quot karatsuba_b karatsuba_power)) (set! karatsuba_b2 (mod karatsuba_b karatsuba_power)) (set! karatsuba_x (karatsuba karatsuba_a2 karatsuba_b2)) (set! karatsuba_y (karatsuba (+ karatsuba_a1 karatsuba_a2) (+ karatsuba_b1 karatsuba_b2))) (set! karatsuba_z (karatsuba karatsuba_a1 karatsuba_b1)) (set! karatsuba_result (+ (+ (* karatsuba_z (int_pow 10 (* 2 karatsuba_m2))) (* (- (- karatsuba_y karatsuba_z) karatsuba_x) karatsuba_power)) karatsuba_x)) (throw (ex-info "return" {:v karatsuba_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (println (str (karatsuba 15463 23489))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
