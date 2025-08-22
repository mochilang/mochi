(ns main (:refer-clojure :exclude [totients solution]))

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

(declare totients solution)

(declare _read_file)

(def ^:dynamic solution_k nil)

(def ^:dynamic solution_phi nil)

(def ^:dynamic solution_total nil)

(def ^:dynamic totients_i nil)

(def ^:dynamic totients_is_prime nil)

(def ^:dynamic totients_j nil)

(def ^:dynamic totients_p nil)

(def ^:dynamic totients_phi nil)

(def ^:dynamic totients_primes nil)

(defn totients [totients_limit]
  (binding [totients_i nil totients_is_prime nil totients_j nil totients_p nil totients_phi nil totients_primes nil] (try (do (set! totients_is_prime []) (set! totients_phi []) (set! totients_primes []) (set! totients_i 0) (while (<= totients_i totients_limit) (do (set! totients_is_prime (conj totients_is_prime true)) (set! totients_phi (conj totients_phi (- totients_i 1))) (set! totients_i (+ totients_i 1)))) (set! totients_i 2) (loop [while_flag_1 true] (when (and while_flag_1 (<= totients_i totients_limit)) (do (when (nth totients_is_prime totients_i) (set! totients_primes (conj totients_primes totients_i))) (set! totients_j 0) (loop [while_flag_2 true] (when (and while_flag_2 (< totients_j (count totients_primes))) (do (set! totients_p (nth totients_primes totients_j)) (cond (> (* totients_i totients_p) totients_limit) (recur false) (= (mod totients_i totients_p) 0) (do (set! totients_phi (assoc totients_phi (* totients_i totients_p) (* (nth totients_phi totients_i) totients_p))) (recur false)) :else (do (set! totients_is_prime (assoc totients_is_prime (* totients_i totients_p) false)) (set! totients_phi (assoc totients_phi (* totients_i totients_p) (* (nth totients_phi totients_i) (- totients_p 1)))) (set! totients_j (+ totients_j 1)) (recur while_flag_2)))))) (set! totients_i (+ totients_i 1)) (cond :else (do))))) (throw (ex-info "return" {:v totients_phi}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_limit]
  (binding [solution_k nil solution_phi nil solution_total nil] (try (do (set! solution_phi (totients solution_limit)) (set! solution_total 0) (set! solution_k 2) (while (<= solution_k solution_limit) (do (set! solution_total (+ solution_total (nth solution_phi solution_k))) (set! solution_k (+ solution_k 1)))) (throw (ex-info "return" {:v solution_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (solution 1000000)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
