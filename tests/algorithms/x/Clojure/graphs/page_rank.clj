(ns main (:refer-clojure :exclude [node_to_string page_rank]))

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

(declare node_to_string page_rank)

(def ^:dynamic main_ci nil)

(def ^:dynamic main_n_in nil)

(def ^:dynamic main_n_out nil)

(def ^:dynamic main_nodes nil)

(def ^:dynamic main_ri nil)

(def ^:dynamic page_rank_i nil)

(def ^:dynamic page_rank_outbounds nil)

(def ^:dynamic page_rank_ranks nil)

(def ^:dynamic page_rank_sum_val nil)

(defn node_to_string [node_to_string_n]
  (try (throw (ex-info "return" {:v (str (str (str (str (str (str "<node=" (:name node_to_string_n)) " inbound=") (:inbound node_to_string_n)) " outbound=") (:outbound node_to_string_n)) ">")})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn page_rank [page_rank_nodes page_rank_limit page_rank_d]
  (binding [page_rank_i nil page_rank_outbounds nil page_rank_ranks nil page_rank_sum_val nil] (try (do (set! page_rank_ranks {}) (doseq [n page_rank_nodes] (set! page_rank_ranks (assoc page_rank_ranks (:name n) 1.0))) (set! page_rank_outbounds {}) (doseq [n page_rank_nodes] (set! page_rank_outbounds (assoc page_rank_outbounds (:name n) (* 1.0 (count (:outbound n)))))) (set! page_rank_i 0) (while (< page_rank_i page_rank_limit) (do (println (str (str "======= Iteration " (str (+ page_rank_i 1))) " =======")) (doseq [n page_rank_nodes] (do (set! page_rank_sum_val 0.0) (doseq [ib (keys (:inbound n))] (set! page_rank_sum_val (+ page_rank_sum_val (quot (get page_rank_ranks ib) (get page_rank_outbounds ib))))) (set! page_rank_ranks (assoc page_rank_ranks (:name n) (+ (- 1.0 page_rank_d) (* page_rank_d page_rank_sum_val)))))) (println page_rank_ranks) (set! page_rank_i (+ page_rank_i 1)))) (throw (ex-info "return" {:v page_rank_ranks}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_names ["A" "B" "C"])

(def ^:dynamic main_graph [[0 1 1] [0 0 1] [1 0 0]])

(def ^:dynamic main_nodes [])

(def ^:dynamic main_ri 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (doseq [name main_names] (def main_nodes (conj main_nodes {:inbound [] :name name :outbound []})))
      (while (< main_ri (count main_graph)) (do (def ^:dynamic main_row (nth main_graph main_ri)) (def ^:dynamic main_ci 0) (while (< main_ci (count main_row)) (do (when (= (nth main_row main_ci) 1) (do (def ^:dynamic main_n_in (nth main_nodes main_ci)) (def main_n_in (assoc main_n_in :inbound (conj (:inbound main_n_in) (nth main_names main_ri)))) (def main_nodes (assoc main_nodes main_ci main_n_in)) (def ^:dynamic main_n_out (nth main_nodes main_ri)) (def main_n_out (assoc main_n_out :outbound (conj (:outbound main_n_out) (nth main_names main_ci)))) (def main_nodes (assoc main_nodes main_ri main_n_out)))) (def main_ci (+ main_ci 1)))) (def main_ri (+ main_ri 1))))
      (println "======= Nodes =======")
      (doseq [n main_nodes] (println main_n))
      (set! main_nodes (page_rank main_nodes 3 0.85))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
