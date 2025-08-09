(ns main (:refer-clojure :exclude [contains jaccard_similarity main]))

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

(declare contains jaccard_similarity main)

(def ^:dynamic contains_i nil)

(def ^:dynamic jaccard_similarity_i nil)

(def ^:dynamic jaccard_similarity_intersection_len nil)

(def ^:dynamic jaccard_similarity_union_len nil)

(def ^:dynamic jaccard_similarity_union_list nil)

(def ^:dynamic jaccard_similarity_val_a nil)

(def ^:dynamic jaccard_similarity_val_b nil)

(def ^:dynamic main_set_a nil)

(def ^:dynamic main_set_b nil)

(defn contains [contains_xs contains_value]
  (binding [contains_i nil] (try (do (set! contains_i 0) (while (< contains_i (count contains_xs)) (do (when (= (nth contains_xs contains_i) contains_value) (throw (ex-info "return" {:v true}))) (set! contains_i (+ contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn jaccard_similarity [jaccard_similarity_set_a jaccard_similarity_set_b jaccard_similarity_alternative_union]
  (binding [jaccard_similarity_i nil jaccard_similarity_intersection_len nil jaccard_similarity_union_len nil jaccard_similarity_union_list nil jaccard_similarity_val_a nil jaccard_similarity_val_b nil] (try (do (set! jaccard_similarity_intersection_len 0) (set! jaccard_similarity_i 0) (while (< jaccard_similarity_i (count jaccard_similarity_set_a)) (do (when (contains jaccard_similarity_set_b (nth jaccard_similarity_set_a jaccard_similarity_i)) (set! jaccard_similarity_intersection_len (+ jaccard_similarity_intersection_len 1))) (set! jaccard_similarity_i (+ jaccard_similarity_i 1)))) (set! jaccard_similarity_union_len 0) (if jaccard_similarity_alternative_union (set! jaccard_similarity_union_len (+ (count jaccard_similarity_set_a) (count jaccard_similarity_set_b))) (do (set! jaccard_similarity_union_list []) (set! jaccard_similarity_i 0) (while (< jaccard_similarity_i (count jaccard_similarity_set_a)) (do (set! jaccard_similarity_val_a (nth jaccard_similarity_set_a jaccard_similarity_i)) (when (not (contains jaccard_similarity_union_list jaccard_similarity_val_a)) (set! jaccard_similarity_union_list (conj jaccard_similarity_union_list jaccard_similarity_val_a))) (set! jaccard_similarity_i (+ jaccard_similarity_i 1)))) (set! jaccard_similarity_i 0) (while (< jaccard_similarity_i (count jaccard_similarity_set_b)) (do (set! jaccard_similarity_val_b (nth jaccard_similarity_set_b jaccard_similarity_i)) (when (not (contains jaccard_similarity_union_list jaccard_similarity_val_b)) (set! jaccard_similarity_union_list (conj jaccard_similarity_union_list jaccard_similarity_val_b))) (set! jaccard_similarity_i (+ jaccard_similarity_i 1)))) (set! jaccard_similarity_union_len (count jaccard_similarity_union_list)))) (throw (ex-info "return" {:v (/ (* 1.0 jaccard_similarity_intersection_len) jaccard_similarity_union_len)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_set_a nil main_set_b nil] (do (set! main_set_a ["a" "b" "c" "d" "e"]) (set! main_set_b ["c" "d" "e" "f" "h" "i"]) (println (jaccard_similarity main_set_a main_set_b false)))))

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
