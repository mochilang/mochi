(ns main (:refer-clojure :exclude [merge strand_sort_rec strand_sort]))

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

(declare merge strand_sort_rec strand_sort)

(def ^:dynamic merge_i nil)

(def ^:dynamic merge_j nil)

(def ^:dynamic merge_result nil)

(def ^:dynamic strand_sort_rec_item nil)

(def ^:dynamic strand_sort_rec_k nil)

(def ^:dynamic strand_sort_rec_last nil)

(def ^:dynamic strand_sort_rec_remaining nil)

(def ^:dynamic strand_sort_rec_solution nil)

(def ^:dynamic strand_sort_rec_sublist nil)

(defn merge [merge_xs merge_ys merge_reverse]
  (binding [merge_i nil merge_j nil merge_result nil] (try (do (set! merge_result []) (set! merge_i 0) (set! merge_j 0) (while (and (< merge_i (count merge_xs)) (< merge_j (count merge_ys))) (if merge_reverse (if (> (nth merge_xs merge_i) (nth merge_ys merge_j)) (do (set! merge_result (conj merge_result (nth merge_xs merge_i))) (set! merge_i (+ merge_i 1))) (do (set! merge_result (conj merge_result (nth merge_ys merge_j))) (set! merge_j (+ merge_j 1)))) (if (< (nth merge_xs merge_i) (nth merge_ys merge_j)) (do (set! merge_result (conj merge_result (nth merge_xs merge_i))) (set! merge_i (+ merge_i 1))) (do (set! merge_result (conj merge_result (nth merge_ys merge_j))) (set! merge_j (+ merge_j 1)))))) (while (< merge_i (count merge_xs)) (do (set! merge_result (conj merge_result (nth merge_xs merge_i))) (set! merge_i (+ merge_i 1)))) (while (< merge_j (count merge_ys)) (do (set! merge_result (conj merge_result (nth merge_ys merge_j))) (set! merge_j (+ merge_j 1)))) (throw (ex-info "return" {:v merge_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn strand_sort_rec [strand_sort_rec_arr strand_sort_rec_reverse strand_sort_rec_solution_p]
  (binding [strand_sort_rec_item nil strand_sort_rec_k nil strand_sort_rec_last nil strand_sort_rec_remaining nil strand_sort_rec_solution nil strand_sort_rec_sublist nil] (try (do (set! strand_sort_rec_solution strand_sort_rec_solution_p) (when (= (count strand_sort_rec_arr) 0) (throw (ex-info "return" {:v strand_sort_rec_solution}))) (set! strand_sort_rec_sublist []) (set! strand_sort_rec_remaining []) (set! strand_sort_rec_sublist (conj strand_sort_rec_sublist (nth strand_sort_rec_arr 0))) (set! strand_sort_rec_last (nth strand_sort_rec_arr 0)) (set! strand_sort_rec_k 1) (while (< strand_sort_rec_k (count strand_sort_rec_arr)) (do (set! strand_sort_rec_item (nth strand_sort_rec_arr strand_sort_rec_k)) (if strand_sort_rec_reverse (if (< strand_sort_rec_item strand_sort_rec_last) (do (set! strand_sort_rec_sublist (conj strand_sort_rec_sublist strand_sort_rec_item)) (set! strand_sort_rec_last strand_sort_rec_item)) (set! strand_sort_rec_remaining (conj strand_sort_rec_remaining strand_sort_rec_item))) (if (> strand_sort_rec_item strand_sort_rec_last) (do (set! strand_sort_rec_sublist (conj strand_sort_rec_sublist strand_sort_rec_item)) (set! strand_sort_rec_last strand_sort_rec_item)) (set! strand_sort_rec_remaining (conj strand_sort_rec_remaining strand_sort_rec_item)))) (set! strand_sort_rec_k (+ strand_sort_rec_k 1)))) (set! strand_sort_rec_solution (merge strand_sort_rec_solution strand_sort_rec_sublist strand_sort_rec_reverse)) (throw (ex-info "return" {:v (strand_sort_rec strand_sort_rec_remaining strand_sort_rec_reverse strand_sort_rec_solution)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn strand_sort [strand_sort_arr strand_sort_reverse]
  (try (throw (ex-info "return" {:v (strand_sort_rec strand_sort_arr strand_sort_reverse [])})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (strand_sort [4 3 5 1 2] false)))
      (println (str (strand_sort [4 3 5 1 2] true)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
