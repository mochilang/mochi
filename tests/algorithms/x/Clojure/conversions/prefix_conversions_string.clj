(ns main (:refer-clojure :exclude [pow add_si_prefix add_binary_prefix]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare pow add_si_prefix add_binary_prefix)

(def ^:dynamic add_binary_prefix_i nil)

(def ^:dynamic add_binary_prefix_num nil)

(def ^:dynamic add_binary_prefix_p nil)

(def ^:dynamic add_si_prefix_i nil)

(def ^:dynamic add_si_prefix_num nil)

(def ^:dynamic add_si_prefix_p nil)

(def ^:dynamic add_si_prefix_prefixes nil)

(def ^:dynamic pow_e nil)

(def ^:dynamic pow_i nil)

(def ^:dynamic pow_result nil)

(def ^:dynamic main_si_positive [{:name "yotta" :exp 24} {:name "zetta" :exp 21} {:name "exa" :exp 18} {:name "peta" :exp 15} {:name "tera" :exp 12} {:name "giga" :exp 9} {:name "mega" :exp 6} {:name "kilo" :exp 3} {:name "hecto" :exp 2} {:name "deca" :exp 1}])

(def ^:dynamic main_si_negative [{:name "deci" :exp (- 1)} {:name "centi" :exp (- 2)} {:name "milli" :exp (- 3)} {:name "micro" :exp (- 6)} {:name "nano" :exp (- 9)} {:name "pico" :exp (- 12)} {:name "femto" :exp (- 15)} {:name "atto" :exp (- 18)} {:name "zepto" :exp (- 21)} {:name "yocto" :exp (- 24)}])

(def ^:dynamic main_binary_prefixes [{:name "yotta" :exp 80} {:name "zetta" :exp 70} {:name "exa" :exp 60} {:name "peta" :exp 50} {:name "tera" :exp 40} {:name "giga" :exp 30} {:name "mega" :exp 20} {:name "kilo" :exp 10}])

(defn pow [pow_base pow_exp]
  (binding [pow_e nil pow_i nil pow_result nil] (try (do (set! pow_result 1.0) (set! pow_e pow_exp) (when (< pow_e 0) (do (set! pow_e (- pow_e)) (set! pow_i 0) (while (< pow_i pow_e) (do (set! pow_result (* pow_result pow_base)) (set! pow_i (+ pow_i 1)))) (throw (ex-info "return" {:v (/ 1.0 pow_result)})))) (set! pow_i 0) (while (< pow_i pow_e) (do (set! pow_result (* pow_result pow_base)) (set! pow_i (+ pow_i 1)))) (throw (ex-info "return" {:v pow_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn add_si_prefix [add_si_prefix_value]
  (binding [add_si_prefix_i nil add_si_prefix_num nil add_si_prefix_p nil add_si_prefix_prefixes nil] (try (do (set! add_si_prefix_prefixes nil) (if (> add_si_prefix_value 0.0) (set! add_si_prefix_prefixes main_si_positive) (set! add_si_prefix_prefixes main_si_negative)) (set! add_si_prefix_i 0) (while (< add_si_prefix_i (count add_si_prefix_prefixes)) (do (set! add_si_prefix_p (nth add_si_prefix_prefixes add_si_prefix_i)) (set! add_si_prefix_num (/ add_si_prefix_value (pow 10.0 (:exp add_si_prefix_p)))) (when (> add_si_prefix_num 1.0) (throw (ex-info "return" {:v (str (str (str add_si_prefix_num) " ") (:name add_si_prefix_p))}))) (set! add_si_prefix_i (+ add_si_prefix_i 1)))) (throw (ex-info "return" {:v (str add_si_prefix_value)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn add_binary_prefix [add_binary_prefix_value]
  (binding [add_binary_prefix_i nil add_binary_prefix_num nil add_binary_prefix_p nil] (try (do (set! add_binary_prefix_i 0) (while (< add_binary_prefix_i (count main_binary_prefixes)) (do (set! add_binary_prefix_p (nth main_binary_prefixes add_binary_prefix_i)) (set! add_binary_prefix_num (/ add_binary_prefix_value (pow 2.0 (:exp add_binary_prefix_p)))) (when (> add_binary_prefix_num 1.0) (throw (ex-info "return" {:v (str (str (str add_binary_prefix_num) " ") (:name add_binary_prefix_p))}))) (set! add_binary_prefix_i (+ add_binary_prefix_i 1)))) (throw (ex-info "return" {:v (str add_binary_prefix_value)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (add_si_prefix 10000.0))
      (println (add_si_prefix 0.005))
      (println (add_binary_prefix 65536.0))
      (println (add_binary_prefix 512.0))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
