(ns main (:refer-clojure :exclude [bead_sort]))

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

(declare bead_sort)

(def ^:dynamic bead_sort_diff nil)

(def ^:dynamic bead_sort_i nil)

(def ^:dynamic bead_sort_j nil)

(def ^:dynamic bead_sort_lower nil)

(def ^:dynamic bead_sort_n nil)

(def ^:dynamic bead_sort_pass nil)

(def ^:dynamic bead_sort_sequence nil)

(def ^:dynamic bead_sort_upper nil)

(defn bead_sort [bead_sort_sequence_p]
  (binding [bead_sort_diff nil bead_sort_i nil bead_sort_j nil bead_sort_lower nil bead_sort_n nil bead_sort_pass nil bead_sort_sequence nil bead_sort_upper nil] (try (do (set! bead_sort_sequence bead_sort_sequence_p) (set! bead_sort_n (count bead_sort_sequence)) (set! bead_sort_i 0) (while (< bead_sort_i bead_sort_n) (do (when (< (nth bead_sort_sequence bead_sort_i) 0) (throw (Exception. "Sequence must be list of non-negative integers"))) (set! bead_sort_i (+ bead_sort_i 1)))) (set! bead_sort_pass 0) (while (< bead_sort_pass bead_sort_n) (do (set! bead_sort_j 0) (while (< bead_sort_j (- bead_sort_n 1)) (do (set! bead_sort_upper (nth bead_sort_sequence bead_sort_j)) (set! bead_sort_lower (nth bead_sort_sequence (+ bead_sort_j 1))) (when (> bead_sort_upper bead_sort_lower) (do (set! bead_sort_diff (- bead_sort_upper bead_sort_lower)) (set! bead_sort_sequence (assoc bead_sort_sequence bead_sort_j (- bead_sort_upper bead_sort_diff))) (set! bead_sort_sequence (assoc bead_sort_sequence (+ bead_sort_j 1) (+ bead_sort_lower bead_sort_diff))))) (set! bead_sort_j (+ bead_sort_j 1)))) (set! bead_sort_pass (+ bead_sort_pass 1)))) (throw (ex-info "return" {:v bead_sort_sequence}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (bead_sort [6 11 12 4 1 5])))
      (println (str (bead_sort [9 8 7 6 5 4 3 2 1])))
      (println (str (bead_sort [5 0 4 3])))
      (println (str (bead_sort [8 2 1])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
