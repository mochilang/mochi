(ns main (:refer-clojure :exclude [to_float sqrt floor hexagonal_num is_pentagonal solution test_hexagonal_num test_is_pentagonal test_solution]))

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

(declare to_float sqrt floor hexagonal_num is_pentagonal solution test_hexagonal_num test_is_pentagonal test_solution)

(declare _read_file)

(def ^:dynamic floor_n nil)

(def ^:dynamic floor_y nil)

(def ^:dynamic is_pentagonal_root nil)

(def ^:dynamic is_pentagonal_val nil)

(def ^:dynamic solution_idx nil)

(def ^:dynamic solution_num nil)

(def ^:dynamic sqrt_guess nil)

(def ^:dynamic sqrt_i nil)

(defn to_float [to_float_x]
  (try (throw (ex-info "return" {:v (* to_float_x 1.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrt [sqrt_x]
  (binding [sqrt_guess nil sqrt_i nil] (try (do (when (<= sqrt_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrt_guess sqrt_x) (set! sqrt_i 0) (while (< sqrt_i 10) (do (set! sqrt_guess (/ (+ sqrt_guess (quot sqrt_x sqrt_guess)) 2.0)) (set! sqrt_i (+ sqrt_i 1)))) (throw (ex-info "return" {:v sqrt_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn floor [floor_x]
  (binding [floor_n nil floor_y nil] (try (do (set! floor_n 0) (set! floor_y floor_x) (while (>= floor_y 1.0) (do (set! floor_y (- floor_y 1.0)) (set! floor_n (+ floor_n 1)))) (throw (ex-info "return" {:v floor_n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hexagonal_num [hexagonal_num_n]
  (try (throw (ex-info "return" {:v (* hexagonal_num_n (- (* 2 hexagonal_num_n) 1))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_pentagonal [is_pentagonal_n]
  (binding [is_pentagonal_root nil is_pentagonal_val nil] (try (do (set! is_pentagonal_root (sqrt (+ 1.0 (* 24.0 (to_float is_pentagonal_n))))) (set! is_pentagonal_val (/ (+ 1.0 is_pentagonal_root) 6.0)) (throw (ex-info "return" {:v (= is_pentagonal_val (to_float (floor is_pentagonal_val)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_start]
  (binding [solution_idx nil solution_num nil] (try (do (set! solution_idx solution_start) (set! solution_num (hexagonal_num solution_idx)) (while (not (is_pentagonal solution_num)) (do (set! solution_idx (+ solution_idx 1)) (set! solution_num (hexagonal_num solution_idx)))) (throw (ex-info "return" {:v solution_num}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_hexagonal_num []
  (do (when (not= (hexagonal_num 143) 40755) (throw (Exception. "hexagonal_num(143) failed"))) (when (not= (hexagonal_num 21) 861) (throw (Exception. "hexagonal_num(21) failed"))) (when (not= (hexagonal_num 10) 190) (throw (Exception. "hexagonal_num(10) failed")))))

(defn test_is_pentagonal []
  (do (when (not (is_pentagonal 330)) (throw (Exception. "330 should be pentagonal"))) (when (is_pentagonal 7683) (throw (Exception. "7683 should not be pentagonal"))) (when (not (is_pentagonal 2380)) (throw (Exception. "2380 should be pentagonal")))))

(defn test_solution []
  (when (not= (solution 144) 1533776805) (throw (Exception. "solution failed"))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (test_hexagonal_num)
      (test_is_pentagonal)
      (test_solution)
      (println (str (mochi_str (solution 144)) " = "))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
