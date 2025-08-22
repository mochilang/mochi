(ns main (:refer-clojure :exclude [solution main]))

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

(declare solution main)

(declare _read_file)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_k nil)

(def ^:dynamic solution_m nil)

(def ^:dynamic solution_n nil)

(def ^:dynamic solution_phi nil)

(def ^:dynamic solution_total nil)

(defn solution [solution_limit]
  (binding [solution_i nil solution_k nil solution_m nil solution_n nil solution_phi nil solution_total nil] (try (do (set! solution_phi []) (set! solution_i 0) (while (<= solution_i solution_limit) (do (set! solution_phi (conj solution_phi solution_i)) (set! solution_i (+ solution_i 1)))) (set! solution_n 2) (while (<= solution_n solution_limit) (do (when (= (nth solution_phi solution_n) solution_n) (do (set! solution_k solution_n) (while (<= solution_k solution_limit) (do (set! solution_phi (assoc solution_phi solution_k (- (nth solution_phi solution_k) (/ (nth solution_phi solution_k) solution_n)))) (set! solution_k (+ solution_k solution_n)))))) (set! solution_n (+ solution_n 1)))) (set! solution_total 0) (set! solution_m 2) (while (<= solution_m solution_limit) (do (set! solution_total (+ solution_total (nth solution_phi solution_m))) (set! solution_m (+ solution_m 1)))) (throw (ex-info "return" {:v solution_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (solution 8)) (println (solution 1000))))

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
