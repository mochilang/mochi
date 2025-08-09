(ns main (:refer-clojure :exclude [floor pow10 round simpson_integration square]))

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

(declare floor pow10 round simpson_integration square)

(def ^:dynamic floor_i nil)

(def ^:dynamic pow10_i nil)

(def ^:dynamic pow10_p nil)

(def ^:dynamic round_m nil)

(def ^:dynamic simpson_integration_h nil)

(def ^:dynamic simpson_integration_i nil)

(def ^:dynamic simpson_integration_r nil)

(def ^:dynamic simpson_integration_result nil)

(def ^:dynamic simpson_integration_x nil)

(def ^:dynamic main_N_STEPS 1000)

(defn floor [floor_x]
  (binding [floor_i nil] (try (do (set! floor_i (long floor_x)) (when (> (double floor_i) floor_x) (set! floor_i (- floor_i 1))) (throw (ex-info "return" {:v (double floor_i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow10 [pow10_n]
  (binding [pow10_i nil pow10_p nil] (try (do (set! pow10_p 1.0) (set! pow10_i 0) (while (< pow10_i pow10_n) (do (set! pow10_p (* pow10_p 10.0)) (set! pow10_i (+ pow10_i 1)))) (throw (ex-info "return" {:v pow10_p}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn round [round_x round_n]
  (binding [round_m nil] (try (do (set! round_m (pow10 round_n)) (throw (ex-info "return" {:v (/ (floor (+ (* round_x round_m) 0.5)) round_m)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn simpson_integration [simpson_integration_f simpson_integration_a simpson_integration_b simpson_integration_precision]
  (binding [simpson_integration_h nil simpson_integration_i nil simpson_integration_r nil simpson_integration_result nil simpson_integration_x nil] (try (do (when (<= simpson_integration_precision 0) (throw (Exception. "precision should be positive"))) (set! simpson_integration_h (quot (- simpson_integration_b simpson_integration_a) (double main_N_STEPS))) (set! simpson_integration_result (+ (simpson_integration_f simpson_integration_a) (simpson_integration_f simpson_integration_b))) (set! simpson_integration_i 1) (while (< simpson_integration_i main_N_STEPS) (do (set! simpson_integration_x (+ simpson_integration_a (* simpson_integration_h (double simpson_integration_i)))) (if (= (mod simpson_integration_i 2) 1) (set! simpson_integration_result (+ simpson_integration_result (* 4.0 (simpson_integration_f simpson_integration_x)))) (set! simpson_integration_result (+ simpson_integration_result (* 2.0 (simpson_integration_f simpson_integration_x))))) (set! simpson_integration_i (+ simpson_integration_i 1)))) (set! simpson_integration_result (* simpson_integration_result (/ simpson_integration_h 3.0))) (set! simpson_integration_r (round simpson_integration_result simpson_integration_precision)) (throw (ex-info "return" {:v simpson_integration_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn square [square_x]
  (try (throw (ex-info "return" {:v (* square_x square_x)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (simpson_integration square 1.0 2.0 3)))
      (println (str (simpson_integration square 3.45 3.2 1)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
