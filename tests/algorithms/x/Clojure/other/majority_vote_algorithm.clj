(ns main (:refer-clojure :exclude [index_of majority_vote main]))

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

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare index_of majority_vote main)

(def ^:dynamic index_of_i nil)

(def ^:dynamic main_votes nil)

(def ^:dynamic majority_vote_candidates nil)

(def ^:dynamic majority_vote_counts nil)

(def ^:dynamic majority_vote_final_counts nil)

(def ^:dynamic majority_vote_i nil)

(def ^:dynamic majority_vote_idx nil)

(def ^:dynamic majority_vote_j nil)

(def ^:dynamic majority_vote_new_candidates nil)

(def ^:dynamic majority_vote_new_counts nil)

(def ^:dynamic majority_vote_result nil)

(def ^:dynamic majority_vote_v nil)

(defn index_of [index_of_xs index_of_x]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_xs)) (do (when (= (nth index_of_xs index_of_i) index_of_x) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v -1}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn majority_vote [majority_vote_votes majority_vote_votes_needed_to_win]
  (binding [majority_vote_candidates nil majority_vote_counts nil majority_vote_final_counts nil majority_vote_i nil majority_vote_idx nil majority_vote_j nil majority_vote_new_candidates nil majority_vote_new_counts nil majority_vote_result nil majority_vote_v nil] (try (do (when (< majority_vote_votes_needed_to_win 2) (throw (ex-info "return" {:v []}))) (set! majority_vote_candidates []) (set! majority_vote_counts []) (set! majority_vote_i 0) (while (< majority_vote_i (count majority_vote_votes)) (do (set! majority_vote_v (nth majority_vote_votes majority_vote_i)) (set! majority_vote_idx (index_of majority_vote_candidates majority_vote_v)) (if (not= majority_vote_idx -1) (set! majority_vote_counts (assoc majority_vote_counts majority_vote_idx (+ (nth majority_vote_counts majority_vote_idx) 1))) (if (< (count majority_vote_candidates) (- majority_vote_votes_needed_to_win 1)) (do (set! majority_vote_candidates (conj majority_vote_candidates majority_vote_v)) (set! majority_vote_counts (conj majority_vote_counts 1))) (do (set! majority_vote_j 0) (while (< majority_vote_j (count majority_vote_counts)) (do (set! majority_vote_counts (assoc majority_vote_counts majority_vote_j (- (nth majority_vote_counts majority_vote_j) 1))) (set! majority_vote_j (+ majority_vote_j 1)))) (set! majority_vote_new_candidates []) (set! majority_vote_new_counts []) (set! majority_vote_j 0) (while (< majority_vote_j (count majority_vote_candidates)) (do (when (> (nth majority_vote_counts majority_vote_j) 0) (do (set! majority_vote_new_candidates (conj majority_vote_new_candidates (nth majority_vote_candidates majority_vote_j))) (set! majority_vote_new_counts (conj majority_vote_new_counts (nth majority_vote_counts majority_vote_j))))) (set! majority_vote_j (+ majority_vote_j 1)))) (set! majority_vote_candidates majority_vote_new_candidates) (set! majority_vote_counts majority_vote_new_counts)))) (set! majority_vote_i (+ majority_vote_i 1)))) (set! majority_vote_final_counts []) (set! majority_vote_j 0) (while (< majority_vote_j (count majority_vote_candidates)) (do (set! majority_vote_final_counts (conj majority_vote_final_counts 0)) (set! majority_vote_j (+ majority_vote_j 1)))) (set! majority_vote_i 0) (while (< majority_vote_i (count majority_vote_votes)) (do (set! majority_vote_v (nth majority_vote_votes majority_vote_i)) (set! majority_vote_idx (index_of majority_vote_candidates majority_vote_v)) (when (not= majority_vote_idx -1) (set! majority_vote_final_counts (assoc majority_vote_final_counts majority_vote_idx (+ (nth majority_vote_final_counts majority_vote_idx) 1)))) (set! majority_vote_i (+ majority_vote_i 1)))) (set! majority_vote_result []) (set! majority_vote_j 0) (while (< majority_vote_j (count majority_vote_candidates)) (do (when (> (* (nth majority_vote_final_counts majority_vote_j) majority_vote_votes_needed_to_win) (count majority_vote_votes)) (set! majority_vote_result (conj majority_vote_result (nth majority_vote_candidates majority_vote_j)))) (set! majority_vote_j (+ majority_vote_j 1)))) (throw (ex-info "return" {:v majority_vote_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_votes nil] (do (set! main_votes [1 2 2 3 1 3 2]) (println (mochi_str (majority_vote main_votes 3))) (println (mochi_str (majority_vote main_votes 2))) (println (mochi_str (majority_vote main_votes 4))))))

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
