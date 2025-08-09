(ns main (:refer-clojure :exclude [binary_multiply binary_mod_multiply main]))

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

(declare binary_multiply binary_mod_multiply main)

(def ^:dynamic binary_mod_multiply_res nil)

(def ^:dynamic binary_mod_multiply_x nil)

(def ^:dynamic binary_mod_multiply_y nil)

(def ^:dynamic binary_multiply_res nil)

(def ^:dynamic binary_multiply_x nil)

(def ^:dynamic binary_multiply_y nil)

(defn binary_multiply [binary_multiply_a binary_multiply_b]
  (binding [binary_multiply_res nil binary_multiply_x nil binary_multiply_y nil] (try (do (set! binary_multiply_x binary_multiply_a) (set! binary_multiply_y binary_multiply_b) (set! binary_multiply_res 0) (while (> binary_multiply_y 0) (do (when (= (mod binary_multiply_y 2) 1) (set! binary_multiply_res (+ binary_multiply_res binary_multiply_x))) (set! binary_multiply_x (+ binary_multiply_x binary_multiply_x)) (set! binary_multiply_y (long (quot binary_multiply_y 2))))) (throw (ex-info "return" {:v binary_multiply_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn binary_mod_multiply [binary_mod_multiply_a binary_mod_multiply_b binary_mod_multiply_modulus]
  (binding [binary_mod_multiply_res nil binary_mod_multiply_x nil binary_mod_multiply_y nil] (try (do (set! binary_mod_multiply_x binary_mod_multiply_a) (set! binary_mod_multiply_y binary_mod_multiply_b) (set! binary_mod_multiply_res 0) (while (> binary_mod_multiply_y 0) (do (when (= (mod binary_mod_multiply_y 2) 1) (set! binary_mod_multiply_res (mod (+ (mod binary_mod_multiply_res binary_mod_multiply_modulus) (mod binary_mod_multiply_x binary_mod_multiply_modulus)) binary_mod_multiply_modulus))) (set! binary_mod_multiply_x (+ binary_mod_multiply_x binary_mod_multiply_x)) (set! binary_mod_multiply_y (long (quot binary_mod_multiply_y 2))))) (throw (ex-info "return" {:v (mod binary_mod_multiply_res binary_mod_multiply_modulus)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (str (binary_multiply 2 3))) (println (str (binary_multiply 5 0))) (println (str (binary_mod_multiply 2 3 5))) (println (str (binary_mod_multiply 10 5 13)))))

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
