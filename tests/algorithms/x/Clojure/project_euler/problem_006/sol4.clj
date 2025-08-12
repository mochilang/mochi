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
  (Integer/parseInt (str s)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare solution)

(def ^:dynamic solution_square_of_sum nil)

(def ^:dynamic solution_sum_first_n nil)

(def ^:dynamic solution_sum_of_squares nil)

(defn solution [solution_n]
  (binding [solution_square_of_sum nil solution_sum_first_n nil solution_sum_of_squares nil] (try (do (set! solution_sum_of_squares (/ (* (* solution_n (+ solution_n 1)) (+ (* 2 solution_n) 1)) 6)) (set! solution_sum_first_n (/ (* solution_n (+ solution_n 1)) 2)) (set! solution_square_of_sum (* solution_sum_first_n solution_sum_first_n)) (throw (ex-info "return" {:v (- solution_square_of_sum solution_sum_of_squares)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (solution 10))
      (println (solution 15))
      (println (solution 20))
      (println (solution 50))
      (println (solution 100))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
