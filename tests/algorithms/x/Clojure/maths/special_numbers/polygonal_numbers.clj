(ns main (:refer-clojure :exclude [polygonal_num main]))

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

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare polygonal_num main)

(def ^:dynamic main_n nil)

(def ^:dynamic main_result nil)

(def ^:dynamic main_sides nil)

(def ^:dynamic polygonal_num_term1 nil)

(def ^:dynamic polygonal_num_term2 nil)

(defn polygonal_num [polygonal_num_n polygonal_num_sides]
  (binding [polygonal_num_term1 nil polygonal_num_term2 nil] (try (do (when (or (< polygonal_num_n 0) (< polygonal_num_sides 3)) (throw (Exception. "Invalid input: num must be >= 0 and sides must be >= 3."))) (set! polygonal_num_term1 (* (* (- polygonal_num_sides 2) polygonal_num_n) polygonal_num_n)) (set! polygonal_num_term2 (* (- polygonal_num_sides 4) polygonal_num_n)) (throw (ex-info "return" {:v (quot (- polygonal_num_term1 polygonal_num_term2) 2)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_n nil main_result nil main_sides nil] (do (set! main_n 5) (set! main_sides 4) (set! main_result (polygonal_num main_n main_sides)) (println (str main_result)))))

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
