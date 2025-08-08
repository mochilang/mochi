(ns main (:refer-clojure :exclude [copy_list longest_subsequence main]))

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

(declare copy_list longest_subsequence main)

(def ^:dynamic copy_list_i nil)

(def ^:dynamic copy_list_res nil)

(def ^:dynamic longest_subsequence_i nil)

(def ^:dynamic longest_subsequence_lis nil)

(def ^:dynamic longest_subsequence_n nil)

(def ^:dynamic longest_subsequence_prev nil)

(def ^:dynamic longest_subsequence_result nil)

(def ^:dynamic longest_subsequence_single nil)

(def ^:dynamic longest_subsequence_temp nil)

(def ^:dynamic longest_subsequence_temp2 nil)

(defn copy_list [copy_list_xs]
  (binding [copy_list_i nil copy_list_res nil] (try (do (set! copy_list_res []) (set! copy_list_i 0) (while (< copy_list_i (count copy_list_xs)) (do (set! copy_list_res (conj copy_list_res (nth copy_list_xs copy_list_i))) (set! copy_list_i (+ copy_list_i 1)))) (throw (ex-info "return" {:v copy_list_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn longest_subsequence [longest_subsequence_arr]
  (binding [longest_subsequence_i nil longest_subsequence_lis nil longest_subsequence_n nil longest_subsequence_prev nil longest_subsequence_result nil longest_subsequence_single nil longest_subsequence_temp nil longest_subsequence_temp2 nil] (try (do (set! longest_subsequence_n (count longest_subsequence_arr)) (set! longest_subsequence_lis []) (set! longest_subsequence_i 0) (while (< longest_subsequence_i longest_subsequence_n) (do (set! longest_subsequence_single []) (set! longest_subsequence_single (conj longest_subsequence_single (nth longest_subsequence_arr longest_subsequence_i))) (set! longest_subsequence_lis (conj longest_subsequence_lis longest_subsequence_single)) (set! longest_subsequence_i (+ longest_subsequence_i 1)))) (set! longest_subsequence_i 1) (while (< longest_subsequence_i longest_subsequence_n) (do (set! longest_subsequence_prev 0) (while (< longest_subsequence_prev longest_subsequence_i) (do (when (and (<= (nth longest_subsequence_arr longest_subsequence_prev) (nth longest_subsequence_arr longest_subsequence_i)) (> (+ (count (nth longest_subsequence_lis longest_subsequence_prev)) 1) (count (nth longest_subsequence_lis longest_subsequence_i)))) (do (set! longest_subsequence_temp (copy_list (nth longest_subsequence_lis longest_subsequence_prev))) (set! longest_subsequence_temp2 (conj longest_subsequence_temp (nth longest_subsequence_arr longest_subsequence_i))) (set! longest_subsequence_lis (assoc longest_subsequence_lis longest_subsequence_i longest_subsequence_temp2)))) (set! longest_subsequence_prev (+ longest_subsequence_prev 1)))) (set! longest_subsequence_i (+ longest_subsequence_i 1)))) (set! longest_subsequence_result []) (set! longest_subsequence_i 0) (while (< longest_subsequence_i longest_subsequence_n) (do (when (> (count (nth longest_subsequence_lis longest_subsequence_i)) (count longest_subsequence_result)) (set! longest_subsequence_result (nth longest_subsequence_lis longest_subsequence_i))) (set! longest_subsequence_i (+ longest_subsequence_i 1)))) (throw (ex-info "return" {:v longest_subsequence_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (str (longest_subsequence [10 22 9 33 21 50 41 60 80]))) (println (str (longest_subsequence [4 8 7 5 1 12 2 3 9]))) (println (str (longest_subsequence [9 8 7 6 5 7]))) (println (str (longest_subsequence [28 26 12 23 35 39]))) (println (str (longest_subsequence [1 1 1]))) (println (str (longest_subsequence [])))))

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
