(ns main (:refer-clojure :exclude [pow_mod solution]))

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

(declare pow_mod solution)

(declare _read_file)

(def ^:dynamic pow_mod_b nil)

(def ^:dynamic pow_mod_e nil)

(def ^:dynamic pow_mod_result nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_modulus nil)

(def ^:dynamic solution_s nil)

(def ^:dynamic solution_total nil)

(defn pow_mod [pow_mod_base pow_mod_exponent pow_mod_modulus]
  (binding [pow_mod_b nil pow_mod_e nil pow_mod_result nil] (try (do (set! pow_mod_result 1) (set! pow_mod_b (mod pow_mod_base pow_mod_modulus)) (set! pow_mod_e pow_mod_exponent) (while (> pow_mod_e 0) (do (when (= (mod pow_mod_e 2) 1) (set! pow_mod_result (mod (* pow_mod_result pow_mod_b) pow_mod_modulus))) (set! pow_mod_b (mod (* pow_mod_b pow_mod_b) pow_mod_modulus)) (set! pow_mod_e (quot pow_mod_e 2)))) (throw (ex-info "return" {:v pow_mod_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution []
  (binding [solution_i nil solution_modulus nil solution_s nil solution_total nil] (try (do (set! solution_modulus 10000000000) (set! solution_total 0) (set! solution_i 1) (while (<= solution_i 1000) (do (set! solution_total (mod (+ solution_total (pow_mod solution_i solution_i solution_modulus)) solution_modulus)) (set! solution_i (+ solution_i 1)))) (set! solution_s (mochi_str solution_total)) (while (< (count solution_s) 10) (set! solution_s (str "0" solution_s))) (throw (ex-info "return" {:v solution_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (solution))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
