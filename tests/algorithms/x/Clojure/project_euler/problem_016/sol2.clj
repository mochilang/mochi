(ns main (:refer-clojure :exclude [solution]))

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

(declare solution)

(declare _read_file)

(def ^:dynamic solution_carry nil)

(def ^:dynamic solution_digits nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_j nil)

(def ^:dynamic solution_k nil)

(def ^:dynamic solution_sum nil)

(def ^:dynamic solution_v nil)

(defn solution [solution_power]
  (binding [solution_carry nil solution_digits nil solution_i nil solution_j nil solution_k nil solution_sum nil solution_v nil] (try (do (set! solution_digits []) (set! solution_digits (conj solution_digits 1)) (set! solution_i 0) (while (< solution_i solution_power) (do (set! solution_carry 0) (set! solution_j 0) (while (< solution_j (count solution_digits)) (do (set! solution_v (+ (* (nth solution_digits solution_j) 2) solution_carry)) (set! solution_digits (assoc solution_digits solution_j (mod solution_v 10))) (set! solution_carry (quot solution_v 10)) (set! solution_j (+ solution_j 1)))) (when (> solution_carry 0) (set! solution_digits (conj solution_digits solution_carry))) (set! solution_i (+ solution_i 1)))) (set! solution_sum 0) (set! solution_k 0) (while (< solution_k (count solution_digits)) (do (set! solution_sum (+ solution_sum (nth solution_digits solution_k))) (set! solution_k (+ solution_k 1)))) (throw (ex-info "return" {:v solution_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (solution 1000)))
      (println (mochi_str (solution 50)))
      (println (mochi_str (solution 20)))
      (println (mochi_str (solution 15)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
