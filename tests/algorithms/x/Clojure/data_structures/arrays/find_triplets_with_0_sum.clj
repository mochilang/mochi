(ns main (:refer-clojure :exclude [sort_triplet contains_triplet contains_int find_triplets_with_0_sum find_triplets_with_0_sum_hashing]))

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

(declare sort_triplet contains_triplet contains_int find_triplets_with_0_sum find_triplets_with_0_sum_hashing)

(def ^:dynamic contains_triplet_item nil)

(def ^:dynamic contains_triplet_same nil)

(def ^:dynamic find_triplets_with_0_sum_a nil)

(def ^:dynamic find_triplets_with_0_sum_b nil)

(def ^:dynamic find_triplets_with_0_sum_c nil)

(def ^:dynamic find_triplets_with_0_sum_hashing_current_sum nil)

(def ^:dynamic find_triplets_with_0_sum_hashing_other nil)

(def ^:dynamic find_triplets_with_0_sum_hashing_output nil)

(def ^:dynamic find_triplets_with_0_sum_hashing_required nil)

(def ^:dynamic find_triplets_with_0_sum_hashing_seen nil)

(def ^:dynamic find_triplets_with_0_sum_hashing_target_sum nil)

(def ^:dynamic find_triplets_with_0_sum_hashing_trip nil)

(def ^:dynamic find_triplets_with_0_sum_n nil)

(def ^:dynamic find_triplets_with_0_sum_result nil)

(def ^:dynamic find_triplets_with_0_sum_trip nil)

(def ^:dynamic sort_triplet_t nil)

(def ^:dynamic sort_triplet_x nil)

(def ^:dynamic sort_triplet_y nil)

(def ^:dynamic sort_triplet_z nil)

(defn sort_triplet [sort_triplet_a sort_triplet_b sort_triplet_c]
  (binding [sort_triplet_t nil sort_triplet_x nil sort_triplet_y nil sort_triplet_z nil] (try (do (set! sort_triplet_x sort_triplet_a) (set! sort_triplet_y sort_triplet_b) (set! sort_triplet_z sort_triplet_c) (when (> sort_triplet_x sort_triplet_y) (do (set! sort_triplet_t sort_triplet_x) (set! sort_triplet_x sort_triplet_y) (set! sort_triplet_y sort_triplet_t))) (when (> sort_triplet_y sort_triplet_z) (do (set! sort_triplet_t sort_triplet_y) (set! sort_triplet_y sort_triplet_z) (set! sort_triplet_z sort_triplet_t))) (when (> sort_triplet_x sort_triplet_y) (do (set! sort_triplet_t sort_triplet_x) (set! sort_triplet_x sort_triplet_y) (set! sort_triplet_y sort_triplet_t))) (throw (ex-info "return" {:v [sort_triplet_x sort_triplet_y sort_triplet_z]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains_triplet [contains_triplet_arr contains_triplet_target]
  (binding [contains_triplet_item nil contains_triplet_same nil] (try (do (dotimes [i (count contains_triplet_arr)] (do (set! contains_triplet_item (nth contains_triplet_arr i)) (set! contains_triplet_same true) (loop [j_seq (count contains_triplet_target)] (when (seq j_seq) (let [j (first j_seq)] (cond (not= (nth contains_triplet_item j) (nth contains_triplet_target j)) (do (set! contains_triplet_same false) (recur nil)) :else (recur (rest j_seq)))))) (when contains_triplet_same (throw (ex-info "return" {:v true}))))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains_int [contains_int_arr contains_int_value]
  (try (do (dotimes [i (count contains_int_arr)] (when (= (nth contains_int_arr i) contains_int_value) (throw (ex-info "return" {:v true})))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn find_triplets_with_0_sum [find_triplets_with_0_sum_nums]
  (binding [find_triplets_with_0_sum_a nil find_triplets_with_0_sum_b nil find_triplets_with_0_sum_c nil find_triplets_with_0_sum_n nil find_triplets_with_0_sum_result nil find_triplets_with_0_sum_trip nil] (try (do (set! find_triplets_with_0_sum_n (count find_triplets_with_0_sum_nums)) (set! find_triplets_with_0_sum_result []) (dotimes [i find_triplets_with_0_sum_n] (doseq [j (range (+ i 1) find_triplets_with_0_sum_n)] (doseq [k (range (+ j 1) find_triplets_with_0_sum_n)] (do (set! find_triplets_with_0_sum_a (nth find_triplets_with_0_sum_nums i)) (set! find_triplets_with_0_sum_b (nth find_triplets_with_0_sum_nums j)) (set! find_triplets_with_0_sum_c (nth find_triplets_with_0_sum_nums k)) (when (= (+ (+ find_triplets_with_0_sum_a find_triplets_with_0_sum_b) find_triplets_with_0_sum_c) 0) (do (set! find_triplets_with_0_sum_trip (sort_triplet find_triplets_with_0_sum_a find_triplets_with_0_sum_b find_triplets_with_0_sum_c)) (when (not (contains_triplet find_triplets_with_0_sum_result find_triplets_with_0_sum_trip)) (set! find_triplets_with_0_sum_result (conj find_triplets_with_0_sum_result find_triplets_with_0_sum_trip))))))))) (throw (ex-info "return" {:v find_triplets_with_0_sum_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_triplets_with_0_sum_hashing [find_triplets_with_0_sum_hashing_arr]
  (binding [find_triplets_with_0_sum_hashing_current_sum nil find_triplets_with_0_sum_hashing_other nil find_triplets_with_0_sum_hashing_output nil find_triplets_with_0_sum_hashing_required nil find_triplets_with_0_sum_hashing_seen nil find_triplets_with_0_sum_hashing_target_sum nil find_triplets_with_0_sum_hashing_trip nil] (try (do (set! find_triplets_with_0_sum_hashing_target_sum 0) (set! find_triplets_with_0_sum_hashing_output []) (dotimes [i (count find_triplets_with_0_sum_hashing_arr)] (do (set! find_triplets_with_0_sum_hashing_seen []) (set! find_triplets_with_0_sum_hashing_current_sum (- find_triplets_with_0_sum_hashing_target_sum (nth find_triplets_with_0_sum_hashing_arr i))) (doseq [j (range (+ i 1) (count find_triplets_with_0_sum_hashing_arr))] (do (set! find_triplets_with_0_sum_hashing_other (nth find_triplets_with_0_sum_hashing_arr j)) (set! find_triplets_with_0_sum_hashing_required (- find_triplets_with_0_sum_hashing_current_sum find_triplets_with_0_sum_hashing_other)) (when (contains_int find_triplets_with_0_sum_hashing_seen find_triplets_with_0_sum_hashing_required) (do (set! find_triplets_with_0_sum_hashing_trip (sort_triplet (nth find_triplets_with_0_sum_hashing_arr i) find_triplets_with_0_sum_hashing_other find_triplets_with_0_sum_hashing_required)) (when (not (contains_triplet find_triplets_with_0_sum_hashing_output find_triplets_with_0_sum_hashing_trip)) (set! find_triplets_with_0_sum_hashing_output (conj find_triplets_with_0_sum_hashing_output find_triplets_with_0_sum_hashing_trip))))) (set! find_triplets_with_0_sum_hashing_seen (conj find_triplets_with_0_sum_hashing_seen find_triplets_with_0_sum_hashing_other)))))) (throw (ex-info "return" {:v find_triplets_with_0_sum_hashing_output}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (find_triplets_with_0_sum [(- 1) 0 1 2 (- 1) (- 4)])))
      (println (str (find_triplets_with_0_sum [])))
      (println (str (find_triplets_with_0_sum [0 0 0])))
      (println (str (find_triplets_with_0_sum [1 2 3 0 (- 1) (- 2) (- 3)])))
      (println (str (find_triplets_with_0_sum_hashing [(- 1) 0 1 2 (- 1) (- 4)])))
      (println (str (find_triplets_with_0_sum_hashing [])))
      (println (str (find_triplets_with_0_sum_hashing [0 0 0])))
      (println (str (find_triplets_with_0_sum_hashing [1 2 3 0 (- 1) (- 2) (- 3)])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
