(ns main (:refer-clojure :exclude [sqrt is_pentagonal pentagonal solution]))

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

(declare sqrt is_pentagonal pentagonal solution)

(declare _read_file)

(def ^:dynamic is_pentagonal_root nil)

(def ^:dynamic is_pentagonal_val nil)

(def ^:dynamic is_pentagonal_val_int nil)

(def ^:dynamic solution_a_idx nil)

(def ^:dynamic solution_b_idx nil)

(def ^:dynamic solution_d nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_pentagonal_i nil)

(def ^:dynamic solution_pentagonal_j nil)

(def ^:dynamic solution_pentagonal_nums nil)

(def ^:dynamic solution_s nil)

(def ^:dynamic sqrt_guess nil)

(def ^:dynamic sqrt_i nil)

(defn sqrt [sqrt_x]
  (binding [sqrt_guess nil sqrt_i nil] (try (do (when (<= sqrt_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrt_guess sqrt_x) (set! sqrt_i 0) (while (< sqrt_i 10) (do (set! sqrt_guess (/ (+ sqrt_guess (quot sqrt_x sqrt_guess)) 2.0)) (set! sqrt_i (+ sqrt_i 1)))) (throw (ex-info "return" {:v sqrt_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_pentagonal [is_pentagonal_n]
  (binding [is_pentagonal_root nil is_pentagonal_val nil is_pentagonal_val_int nil] (try (do (set! is_pentagonal_root (sqrt (+ 1.0 (* 24.0 (* 1.0 is_pentagonal_n))))) (set! is_pentagonal_val (/ (+ 1.0 is_pentagonal_root) 6.0)) (set! is_pentagonal_val_int (toi is_pentagonal_val)) (throw (ex-info "return" {:v (= is_pentagonal_val (* 1.0 is_pentagonal_val_int))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pentagonal [pentagonal_k]
  (try (throw (ex-info "return" {:v (quot (* pentagonal_k (- (* 3 pentagonal_k) 1)) 2)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn solution [solution_limit]
  (binding [solution_a_idx nil solution_b_idx nil solution_d nil solution_i nil solution_pentagonal_i nil solution_pentagonal_j nil solution_pentagonal_nums nil solution_s nil] (try (do (set! solution_pentagonal_nums []) (set! solution_i 1) (while (< solution_i solution_limit) (do (set! solution_pentagonal_nums (conj solution_pentagonal_nums (pentagonal solution_i))) (set! solution_i (+ solution_i 1)))) (set! solution_a_idx 0) (while (< solution_a_idx (count solution_pentagonal_nums)) (do (set! solution_pentagonal_i (nth solution_pentagonal_nums solution_a_idx)) (set! solution_b_idx solution_a_idx) (while (< solution_b_idx (count solution_pentagonal_nums)) (do (set! solution_pentagonal_j (nth solution_pentagonal_nums solution_b_idx)) (set! solution_s (+ solution_pentagonal_i solution_pentagonal_j)) (set! solution_d (- solution_pentagonal_j solution_pentagonal_i)) (when (and (is_pentagonal solution_s) (is_pentagonal solution_d)) (throw (ex-info "return" {:v solution_d}))) (set! solution_b_idx (+ solution_b_idx 1)))) (set! solution_a_idx (+ solution_a_idx 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_result nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_result) (constantly (solution 5000)))
      (println (str "solution() = " (mochi_str main_result)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
