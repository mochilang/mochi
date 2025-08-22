(ns main (:refer-clojure :exclude [intSqrt continuousFractionPeriod solution main]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare intSqrt continuousFractionPeriod solution main)

(declare _read_file)

(def ^:dynamic continuousFractionPeriod_a nil)

(def ^:dynamic continuousFractionPeriod_a0 nil)

(def ^:dynamic continuousFractionPeriod_d nil)

(def ^:dynamic continuousFractionPeriod_m nil)

(def ^:dynamic continuousFractionPeriod_period nil)

(def ^:dynamic count_v nil)

(def ^:dynamic intSqrt_x nil)

(def ^:dynamic intSqrt_y nil)

(def ^:dynamic main_n nil)

(def ^:dynamic main_nStr nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_p nil)

(def ^:dynamic solution_r nil)

(defn intSqrt [intSqrt_n]
  (binding [intSqrt_x nil intSqrt_y nil] (try (do (when (= intSqrt_n 0) (throw (ex-info "return" {:v 0}))) (set! intSqrt_x intSqrt_n) (set! intSqrt_y (/ (+ intSqrt_x 1) 2)) (while (< intSqrt_y intSqrt_x) (do (set! intSqrt_x intSqrt_y) (set! intSqrt_y (/ (+ intSqrt_x (/ intSqrt_n intSqrt_x)) 2)))) (throw (ex-info "return" {:v intSqrt_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn continuousFractionPeriod [continuousFractionPeriod_n]
  (binding [continuousFractionPeriod_a nil continuousFractionPeriod_a0 nil continuousFractionPeriod_d nil continuousFractionPeriod_m nil continuousFractionPeriod_period nil] (try (do (set! continuousFractionPeriod_m 0) (set! continuousFractionPeriod_d 1) (set! continuousFractionPeriod_a0 (intSqrt continuousFractionPeriod_n)) (set! continuousFractionPeriod_a continuousFractionPeriod_a0) (set! continuousFractionPeriod_period 0) (while (not= continuousFractionPeriod_a (* 2 continuousFractionPeriod_a0)) (do (set! continuousFractionPeriod_m (- (* continuousFractionPeriod_d continuousFractionPeriod_a) continuousFractionPeriod_m)) (set! continuousFractionPeriod_d (/ (- continuousFractionPeriod_n (* continuousFractionPeriod_m continuousFractionPeriod_m)) continuousFractionPeriod_d)) (set! continuousFractionPeriod_a (/ (+ continuousFractionPeriod_a0 continuousFractionPeriod_m) continuousFractionPeriod_d)) (set! continuousFractionPeriod_period (+ continuousFractionPeriod_period 1)))) (throw (ex-info "return" {:v continuousFractionPeriod_period}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_n]
  (binding [count_v nil solution_i nil solution_p nil solution_r nil] (try (do (set! count_v 0) (doseq [solution_i (range 2 (+ solution_n 1))] (do (set! solution_r (intSqrt solution_i)) (when (not= (* solution_r solution_r) solution_i) (do (set! solution_p (continuousFractionPeriod solution_i)) (when (= (mod solution_p 2) 1) (set! count_v (+ count_v 1))))))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_n nil main_nStr nil] (do (set! main_nStr (read-line)) (set! main_n (toi main_nStr)) (println (solution main_n)))))

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
