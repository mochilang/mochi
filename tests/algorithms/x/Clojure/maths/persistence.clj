(ns main (:refer-clojure :exclude [multiplicative_persistence additive_persistence test_persistence main]))

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

(declare multiplicative_persistence additive_persistence test_persistence main)

(def ^:dynamic additive_persistence_digit nil)

(def ^:dynamic additive_persistence_n nil)

(def ^:dynamic additive_persistence_steps nil)

(def ^:dynamic additive_persistence_temp nil)

(def ^:dynamic additive_persistence_total nil)

(def ^:dynamic multiplicative_persistence_digit nil)

(def ^:dynamic multiplicative_persistence_n nil)

(def ^:dynamic multiplicative_persistence_product nil)

(def ^:dynamic multiplicative_persistence_steps nil)

(def ^:dynamic multiplicative_persistence_temp nil)

(defn multiplicative_persistence [multiplicative_persistence_num]
  (binding [multiplicative_persistence_digit nil multiplicative_persistence_n nil multiplicative_persistence_product nil multiplicative_persistence_steps nil multiplicative_persistence_temp nil] (try (do (when (< multiplicative_persistence_num 0) (throw (Exception. "multiplicative_persistence() does not accept negative values"))) (set! multiplicative_persistence_steps 0) (set! multiplicative_persistence_n multiplicative_persistence_num) (while (>= multiplicative_persistence_n 10) (do (set! multiplicative_persistence_product 1) (set! multiplicative_persistence_temp multiplicative_persistence_n) (while (> multiplicative_persistence_temp 0) (do (set! multiplicative_persistence_digit (mod multiplicative_persistence_temp 10)) (set! multiplicative_persistence_product (* multiplicative_persistence_product multiplicative_persistence_digit)) (set! multiplicative_persistence_temp (quot multiplicative_persistence_temp 10)))) (set! multiplicative_persistence_n multiplicative_persistence_product) (set! multiplicative_persistence_steps (+ multiplicative_persistence_steps 1)))) (throw (ex-info "return" {:v multiplicative_persistence_steps}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn additive_persistence [additive_persistence_num]
  (binding [additive_persistence_digit nil additive_persistence_n nil additive_persistence_steps nil additive_persistence_temp nil additive_persistence_total nil] (try (do (when (< additive_persistence_num 0) (throw (Exception. "additive_persistence() does not accept negative values"))) (set! additive_persistence_steps 0) (set! additive_persistence_n additive_persistence_num) (while (>= additive_persistence_n 10) (do (set! additive_persistence_total 0) (set! additive_persistence_temp additive_persistence_n) (while (> additive_persistence_temp 0) (do (set! additive_persistence_digit (mod additive_persistence_temp 10)) (set! additive_persistence_total (+ additive_persistence_total additive_persistence_digit)) (set! additive_persistence_temp (quot additive_persistence_temp 10)))) (set! additive_persistence_n additive_persistence_total) (set! additive_persistence_steps (+ additive_persistence_steps 1)))) (throw (ex-info "return" {:v additive_persistence_steps}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_persistence []
  (do (when (not= (multiplicative_persistence 217) 2) (throw (Exception. "multiplicative_persistence failed"))) (when (not= (additive_persistence 199) 3) (throw (Exception. "additive_persistence failed")))))

(defn main []
  (do (test_persistence) (println (str (multiplicative_persistence 217))) (println (str (additive_persistence 199)))))

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
