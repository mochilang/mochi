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
  (Integer/parseInt (str s)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare solution main)

(def ^:dynamic solution_a nil)

(def ^:dynamic solution_b nil)

(def ^:dynamic solution_c nil)

(def ^:dynamic solution_candidate nil)

(def ^:dynamic solution_product nil)

(defn solution [solution_n]
  (binding [solution_a nil solution_b nil solution_c nil solution_candidate nil solution_product nil] (try (do (set! solution_product (- 1)) (set! solution_candidate 0) (set! solution_a 1) (while (< solution_a (/ solution_n 3)) (do (set! solution_b (/ (- (* solution_n solution_n) (* (* 2 solution_a) solution_n)) (- (* 2 solution_n) (* 2 solution_a)))) (set! solution_c (- (- solution_n solution_a) solution_b)) (when (= (* solution_c solution_c) (+ (* solution_a solution_a) (* solution_b solution_b))) (do (set! solution_candidate (* (* solution_a solution_b) solution_c)) (when (> solution_candidate solution_product) (set! solution_product solution_candidate)))) (set! solution_a (+ solution_a 1)))) (throw (ex-info "return" {:v solution_product}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (println (str "solution() = " (str (solution 1000)))))

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
