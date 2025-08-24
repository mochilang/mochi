(ns main (:refer-clojure :exclude [binomial_coefficient catalan_number factorial binary_tree_count]))

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

(declare binomial_coefficient catalan_number factorial binary_tree_count)

(declare _read_file)

(def ^:dynamic binomial_coefficient_i nil)

(def ^:dynamic binomial_coefficient_kk nil)

(def ^:dynamic binomial_coefficient_result nil)

(def ^:dynamic factorial_i nil)

(def ^:dynamic factorial_result nil)

(defn binomial_coefficient [binomial_coefficient_n binomial_coefficient_k]
  (binding [binomial_coefficient_i nil binomial_coefficient_kk nil binomial_coefficient_result nil] (try (do (set! binomial_coefficient_result 1) (set! binomial_coefficient_kk binomial_coefficient_k) (when (> binomial_coefficient_k (- binomial_coefficient_n binomial_coefficient_k)) (set! binomial_coefficient_kk (- binomial_coefficient_n binomial_coefficient_k))) (dotimes [binomial_coefficient_i binomial_coefficient_kk] (do (set! binomial_coefficient_result (* binomial_coefficient_result (- binomial_coefficient_n binomial_coefficient_i))) (set! binomial_coefficient_result (quot binomial_coefficient_result (+ binomial_coefficient_i 1))))) (throw (ex-info "return" {:v binomial_coefficient_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn catalan_number [catalan_number_node_count]
  (try (throw (ex-info "return" {:v (quot (binomial_coefficient (* 2 catalan_number_node_count) catalan_number_node_count) (+ catalan_number_node_count 1))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn factorial [factorial_n]
  (binding [factorial_i nil factorial_result nil] (try (do (when (< factorial_n 0) (do (println "factorial() not defined for negative values") (throw (ex-info "return" {:v 0})))) (set! factorial_result 1) (doseq [factorial_i (range 1 (+ factorial_n 1))] (set! factorial_result (* factorial_result factorial_i))) (throw (ex-info "return" {:v factorial_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn binary_tree_count [binary_tree_count_node_count]
  (try (throw (ex-info "return" {:v (* (catalan_number binary_tree_count_node_count) (factorial binary_tree_count_node_count))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_input_str nil)

(def ^:dynamic main_node_count nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "Enter the number of nodes:")
      (alter-var-root (var main_input_str) (constantly (read-line)))
      (alter-var-root (var main_node_count) (constantly (toi main_input_str)))
      (if (<= main_node_count 0) (println "We need some nodes to work with.") (do (def ^:dynamic main_bst (catalan_number main_node_count)) (def ^:dynamic main_bt (binary_tree_count main_node_count)) (println "Given" main_node_count "nodes, there are" main_bt "binary trees and" main_bst "binary search trees.")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
