(ns main (:refer-clojure :exclude [pivot kth_number]))

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

(declare pivot kth_number)

(def ^:dynamic kth_number_big nil)

(def ^:dynamic kth_number_e nil)

(def ^:dynamic kth_number_i nil)

(def ^:dynamic kth_number_p nil)

(def ^:dynamic kth_number_small nil)

(defn pivot [pivot_lst]
  (try (throw (ex-info "return" {:v (nth pivot_lst 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn kth_number [kth_number_lst kth_number_k]
  (binding [kth_number_big nil kth_number_e nil kth_number_i nil kth_number_p nil kth_number_small nil] (try (do (set! kth_number_p (pivot kth_number_lst)) (set! kth_number_small []) (set! kth_number_big []) (set! kth_number_i 0) (while (< kth_number_i (count kth_number_lst)) (do (set! kth_number_e (nth kth_number_lst kth_number_i)) (if (< kth_number_e kth_number_p) (set! kth_number_small (conj kth_number_small kth_number_e)) (when (> kth_number_e kth_number_p) (set! kth_number_big (conj kth_number_big kth_number_e)))) (set! kth_number_i (+ kth_number_i 1)))) (if (= (count kth_number_small) (- kth_number_k 1)) (throw (ex-info "return" {:v kth_number_p})) (if (< (count kth_number_small) (- kth_number_k 1)) (throw (ex-info "return" {:v (kth_number kth_number_big (- (- kth_number_k (count kth_number_small)) 1))})) (throw (ex-info "return" {:v (kth_number kth_number_small kth_number_k)}))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (kth_number [2 1 3 4 5] 3)))
      (println (str (kth_number [2 1 3 4 5] 1)))
      (println (str (kth_number [2 1 3 4 5] 5)))
      (println (str (kth_number [3 2 5 6 7 8] 2)))
      (println (str (kth_number [25 21 98 100 76 22 43 60 89 87] 4)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
