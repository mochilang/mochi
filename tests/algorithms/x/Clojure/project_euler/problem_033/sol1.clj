(ns main (:refer-clojure :exclude [gcd is_digit_cancelling find_fractions solution main]))

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

(declare gcd is_digit_cancelling find_fractions solution main)

(declare _read_file)

(def ^:dynamic find_fractions_den nil)

(def ^:dynamic find_fractions_num nil)

(def ^:dynamic find_fractions_sols nil)

(def ^:dynamic gcd_t nil)

(def ^:dynamic gcd_x nil)

(def ^:dynamic gcd_y nil)

(def ^:dynamic is_digit_cancelling_den_tens nil)

(def ^:dynamic is_digit_cancelling_den_unit nil)

(def ^:dynamic is_digit_cancelling_num_tens nil)

(def ^:dynamic is_digit_cancelling_num_unit nil)

(def ^:dynamic solution_den_prod nil)

(def ^:dynamic solution_f nil)

(def ^:dynamic solution_fracs nil)

(def ^:dynamic solution_g nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_num_prod nil)

(defn gcd [gcd_a gcd_b]
  (binding [gcd_t nil gcd_x nil gcd_y nil] (try (do (set! gcd_x (if (< gcd_a 0) (- gcd_a) gcd_a)) (set! gcd_y (if (< gcd_b 0) (- gcd_b) gcd_b)) (while (not= gcd_y 0) (do (set! gcd_t (mod gcd_x gcd_y)) (set! gcd_x gcd_y) (set! gcd_y gcd_t))) (throw (ex-info "return" {:v gcd_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_digit_cancelling [is_digit_cancelling_num is_digit_cancelling_den]
  (binding [is_digit_cancelling_den_tens nil is_digit_cancelling_den_unit nil is_digit_cancelling_num_tens nil is_digit_cancelling_num_unit nil] (try (do (when (>= is_digit_cancelling_num is_digit_cancelling_den) (throw (ex-info "return" {:v false}))) (set! is_digit_cancelling_num_unit (mod is_digit_cancelling_num 10)) (set! is_digit_cancelling_num_tens (quot is_digit_cancelling_num 10)) (set! is_digit_cancelling_den_unit (mod is_digit_cancelling_den 10)) (set! is_digit_cancelling_den_tens (quot is_digit_cancelling_den 10)) (when (not= is_digit_cancelling_num_unit is_digit_cancelling_den_tens) (throw (ex-info "return" {:v false}))) (if (= is_digit_cancelling_den_unit 0) false (= (* is_digit_cancelling_num is_digit_cancelling_den_unit) (* is_digit_cancelling_num_tens is_digit_cancelling_den)))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_fractions []
  (binding [find_fractions_den nil find_fractions_num nil find_fractions_sols nil] (try (do (set! find_fractions_sols []) (set! find_fractions_num 10) (while (< find_fractions_num 100) (do (set! find_fractions_den (+ find_fractions_num 1)) (while (< find_fractions_den 100) (do (when (is_digit_cancelling find_fractions_num find_fractions_den) (set! find_fractions_sols (conj find_fractions_sols {:den find_fractions_den :num find_fractions_num}))) (set! find_fractions_den (+ find_fractions_den 1)))) (set! find_fractions_num (+ find_fractions_num 1)))) (throw (ex-info "return" {:v find_fractions_sols}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution []
  (binding [solution_den_prod nil solution_f nil solution_fracs nil solution_g nil solution_i nil solution_num_prod nil] (try (do (set! solution_fracs (find_fractions)) (set! solution_num_prod 1) (set! solution_den_prod 1) (set! solution_i 0) (while (< solution_i (count solution_fracs)) (do (set! solution_f (nth solution_fracs solution_i)) (set! solution_num_prod (* solution_num_prod (:num solution_f))) (set! solution_den_prod (* solution_den_prod (:den solution_f))) (set! solution_i (+ solution_i 1)))) (set! solution_g (gcd solution_num_prod solution_den_prod)) (throw (ex-info "return" {:v (quot solution_den_prod solution_g)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (println (mochi_str (solution))))

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
