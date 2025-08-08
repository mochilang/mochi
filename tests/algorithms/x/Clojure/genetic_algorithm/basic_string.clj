(ns main (:refer-clojure :exclude [evaluate crossover mutate main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare evaluate crossover mutate main)

(def ^:dynamic crossover_child1 nil)

(def ^:dynamic crossover_child2 nil)

(def ^:dynamic crossover_cut nil)

(def ^:dynamic evaluate_i nil)

(def ^:dynamic evaluate_score nil)

(def ^:dynamic main_mut nil)

(def ^:dynamic main_pair nil)

(def ^:dynamic mutate_gene nil)

(defn evaluate [evaluate_item evaluate_target]
  (binding [evaluate_i nil evaluate_score nil] (try (do (set! evaluate_score 0) (set! evaluate_i 0) (while (and (< evaluate_i (count evaluate_item)) (< evaluate_i (count evaluate_target))) (do (when (= (subs evaluate_item evaluate_i (min (+ evaluate_i 1) (count evaluate_item))) (subs evaluate_target evaluate_i (min (+ evaluate_i 1) (count evaluate_target)))) (set! evaluate_score (+ evaluate_score 1))) (set! evaluate_i (+ evaluate_i 1)))) (throw (ex-info "return" {:v evaluate_score}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn crossover [crossover_parent1 crossover_parent2]
  (binding [crossover_child1 nil crossover_child2 nil crossover_cut nil] (try (do (set! crossover_cut (quot (count crossover_parent1) 2)) (set! crossover_child1 (str (subs crossover_parent1 0 (min crossover_cut (count crossover_parent1))) (subs crossover_parent2 crossover_cut (min (count crossover_parent2) (count crossover_parent2))))) (set! crossover_child2 (str (subs crossover_parent2 0 (min crossover_cut (count crossover_parent2))) (subs crossover_parent1 crossover_cut (min (count crossover_parent1) (count crossover_parent1))))) (throw (ex-info "return" {:v {:first crossover_child1 :second crossover_child2}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mutate [mutate_child mutate_genes]
  (binding [mutate_gene nil] (try (do (when (= (count mutate_child) 0) (throw (ex-info "return" {:v mutate_child}))) (set! mutate_gene (nth mutate_genes 0)) (throw (ex-info "return" {:v (str (subs mutate_child 0 (min (- (count mutate_child) 1) (count mutate_child))) mutate_gene)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_mut nil main_pair nil] (do (println (str (evaluate "Helxo Worlx" "Hello World"))) (set! main_pair (crossover "123456" "abcdef")) (println (:first main_pair)) (println (:second main_pair)) (set! main_mut (mutate "123456" ["A" "B" "C" "D" "E" "F"])) (println main_mut))))

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
