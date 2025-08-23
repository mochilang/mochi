(ns main (:refer-clojure :exclude [int_sqrt pythagorean_triple max_perimeter solution main]))

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

(declare int_sqrt pythagorean_triple max_perimeter solution main)

(declare _read_file)

(def ^:dynamic count_v nil)

(def ^:dynamic int_sqrt_high nil)

(def ^:dynamic int_sqrt_low nil)

(def ^:dynamic int_sqrt_mid nil)

(def ^:dynamic int_sqrt_sq nil)

(def ^:dynamic main_best nil)

(def ^:dynamic main_s100 nil)

(def ^:dynamic main_s200 nil)

(def ^:dynamic max_perimeter_best_p nil)

(def ^:dynamic max_perimeter_max_count nil)

(def ^:dynamic max_perimeter_p nil)

(def ^:dynamic pythagorean_triple_base nil)

(def ^:dynamic pythagorean_triple_hyp nil)

(def ^:dynamic pythagorean_triple_hyp_sq nil)

(def ^:dynamic pythagorean_triple_perimeter nil)

(def ^:dynamic pythagorean_triple_perpendicular nil)

(def ^:dynamic pythagorean_triple_triplets nil)

(def ^:dynamic solution_triplets nil)

(defn int_sqrt [int_sqrt_n]
  (binding [int_sqrt_high nil int_sqrt_low nil int_sqrt_mid nil int_sqrt_sq nil] (try (do (set! int_sqrt_low 0) (set! int_sqrt_high int_sqrt_n) (while (<= int_sqrt_low int_sqrt_high) (do (set! int_sqrt_mid (quot (+ int_sqrt_low int_sqrt_high) 2)) (set! int_sqrt_sq (* int_sqrt_mid int_sqrt_mid)) (when (= int_sqrt_sq int_sqrt_n) (throw (ex-info "return" {:v int_sqrt_mid}))) (if (< int_sqrt_sq int_sqrt_n) (set! int_sqrt_low (+ int_sqrt_mid 1)) (set! int_sqrt_high (- int_sqrt_mid 1))))) (throw (ex-info "return" {:v int_sqrt_high}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pythagorean_triple [max_perimeter_v]
  (binding [pythagorean_triple_base nil pythagorean_triple_hyp nil pythagorean_triple_hyp_sq nil pythagorean_triple_perimeter nil pythagorean_triple_perpendicular nil pythagorean_triple_triplets nil] (try (do (set! pythagorean_triple_triplets {}) (set! pythagorean_triple_base 1) (while (<= pythagorean_triple_base max_perimeter_v) (do (set! pythagorean_triple_perpendicular pythagorean_triple_base) (while (<= pythagorean_triple_perpendicular max_perimeter_v) (do (set! pythagorean_triple_hyp_sq (+ (* pythagorean_triple_base pythagorean_triple_base) (* pythagorean_triple_perpendicular pythagorean_triple_perpendicular))) (set! pythagorean_triple_hyp (int_sqrt pythagorean_triple_hyp_sq)) (when (= (* pythagorean_triple_hyp pythagorean_triple_hyp) pythagorean_triple_hyp_sq) (do (set! pythagorean_triple_perimeter (+ (+ pythagorean_triple_base pythagorean_triple_perpendicular) pythagorean_triple_hyp)) (when (<= pythagorean_triple_perimeter max_perimeter_v) (if (in pythagorean_triple_perimeter pythagorean_triple_triplets) (set! pythagorean_triple_triplets (assoc pythagorean_triple_triplets pythagorean_triple_perimeter (+ (get pythagorean_triple_triplets pythagorean_triple_perimeter) 1))) (set! pythagorean_triple_triplets (assoc pythagorean_triple_triplets pythagorean_triple_perimeter 1)))))) (set! pythagorean_triple_perpendicular (+ pythagorean_triple_perpendicular 1)))) (set! pythagorean_triple_base (+ pythagorean_triple_base 1)))) (throw (ex-info "return" {:v pythagorean_triple_triplets}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn max_perimeter [max_perimeter_counts]
  (binding [count_v nil max_perimeter_best_p nil max_perimeter_max_count nil max_perimeter_p nil] (try (do (set! max_perimeter_best_p 0) (set! max_perimeter_max_count 0) (doseq [max_perimeter_p (keys max_perimeter_counts)] (do (set! count_v (get max_perimeter_counts max_perimeter_p)) (when (or (> count_v max_perimeter_max_count) (and (= count_v max_perimeter_max_count) (> max_perimeter_p max_perimeter_best_p))) (do (set! max_perimeter_max_count count_v) (set! max_perimeter_best_p max_perimeter_p))))) (throw (ex-info "return" {:v max_perimeter_best_p}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_n]
  (binding [solution_triplets nil] (try (do (set! solution_triplets (pythagorean_triple solution_n)) (throw (ex-info "return" {:v (max_perimeter solution_triplets)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_best nil main_s100 nil main_s200 nil] (do (set! main_s200 (max_perimeter (pythagorean_triple 200))) (set! main_s100 (max_perimeter (pythagorean_triple 100))) (println (mochi_str main_s100)) (println (mochi_str main_s200)) (set! main_best (max_perimeter (pythagorean_triple 1000))) (println (mochi_str main_best)) (println (str (str "Perimeter " (mochi_str main_best)) " has maximum solutions")))))

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
