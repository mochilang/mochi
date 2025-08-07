(ns main (:refer-clojure :exclude [slice_list count_inversions_bf count_cross_inversions count_inversions_recursive]))

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

(declare slice_list count_inversions_bf count_cross_inversions count_inversions_recursive)

(def ^:dynamic count_cross_inversions_i nil)

(def ^:dynamic count_cross_inversions_inv nil)

(def ^:dynamic count_cross_inversions_j nil)

(def ^:dynamic count_cross_inversions_r nil)

(def ^:dynamic count_inversions_bf_i nil)

(def ^:dynamic count_inversions_bf_inv nil)

(def ^:dynamic count_inversions_bf_j nil)

(def ^:dynamic count_inversions_bf_n nil)

(def ^:dynamic count_inversions_recursive_mid nil)

(def ^:dynamic count_inversions_recursive_p nil)

(def ^:dynamic count_inversions_recursive_q nil)

(def ^:dynamic count_inversions_recursive_res_cross nil)

(def ^:dynamic count_inversions_recursive_res_p nil)

(def ^:dynamic count_inversions_recursive_res_q nil)

(def ^:dynamic count_inversions_recursive_total nil)

(def ^:dynamic main_arr_1 nil)

(def ^:dynamic slice_list_k nil)

(def ^:dynamic slice_list_res nil)

(defn slice_list [slice_list_arr slice_list_start slice_list_end]
  (binding [slice_list_k nil slice_list_res nil] (try (do (set! slice_list_res []) (set! slice_list_k slice_list_start) (while (< slice_list_k slice_list_end) (do (set! slice_list_res (conj slice_list_res (nth slice_list_arr slice_list_k))) (set! slice_list_k (+ slice_list_k 1)))) (throw (ex-info "return" {:v slice_list_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn count_inversions_bf [count_inversions_bf_arr]
  (binding [count_inversions_bf_i nil count_inversions_bf_inv nil count_inversions_bf_j nil count_inversions_bf_n nil] (try (do (set! count_inversions_bf_n (count count_inversions_bf_arr)) (set! count_inversions_bf_inv 0) (set! count_inversions_bf_i 0) (while (< count_inversions_bf_i (- count_inversions_bf_n 1)) (do (set! count_inversions_bf_j (+ count_inversions_bf_i 1)) (while (< count_inversions_bf_j count_inversions_bf_n) (do (when (> (nth count_inversions_bf_arr count_inversions_bf_i) (nth count_inversions_bf_arr count_inversions_bf_j)) (set! count_inversions_bf_inv (+ count_inversions_bf_inv 1))) (set! count_inversions_bf_j (+ count_inversions_bf_j 1)))) (set! count_inversions_bf_i (+ count_inversions_bf_i 1)))) (throw (ex-info "return" {:v count_inversions_bf_inv}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn count_cross_inversions [count_cross_inversions_p count_cross_inversions_q]
  (binding [count_cross_inversions_i nil count_cross_inversions_inv nil count_cross_inversions_j nil count_cross_inversions_r nil] (try (do (set! count_cross_inversions_r []) (set! count_cross_inversions_i 0) (set! count_cross_inversions_j 0) (set! count_cross_inversions_inv 0) (while (and (< count_cross_inversions_i (count count_cross_inversions_p)) (< count_cross_inversions_j (count count_cross_inversions_q))) (if (> (nth count_cross_inversions_p count_cross_inversions_i) (nth count_cross_inversions_q count_cross_inversions_j)) (do (set! count_cross_inversions_inv (+ count_cross_inversions_inv (- (count count_cross_inversions_p) count_cross_inversions_i))) (set! count_cross_inversions_r (conj count_cross_inversions_r (nth count_cross_inversions_q count_cross_inversions_j))) (set! count_cross_inversions_j (+ count_cross_inversions_j 1))) (do (set! count_cross_inversions_r (conj count_cross_inversions_r (nth count_cross_inversions_p count_cross_inversions_i))) (set! count_cross_inversions_i (+ count_cross_inversions_i 1))))) (if (< count_cross_inversions_i (count count_cross_inversions_p)) (set! count_cross_inversions_r (concat count_cross_inversions_r (slice_list count_cross_inversions_p count_cross_inversions_i (count count_cross_inversions_p)))) (set! count_cross_inversions_r (concat count_cross_inversions_r (slice_list count_cross_inversions_q count_cross_inversions_j (count count_cross_inversions_q))))) (throw (ex-info "return" {:v {:arr count_cross_inversions_r :inv count_cross_inversions_inv}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn count_inversions_recursive [count_inversions_recursive_arr]
  (binding [count_inversions_recursive_mid nil count_inversions_recursive_p nil count_inversions_recursive_q nil count_inversions_recursive_res_cross nil count_inversions_recursive_res_p nil count_inversions_recursive_res_q nil count_inversions_recursive_total nil] (try (do (when (<= (count count_inversions_recursive_arr) 1) (throw (ex-info "return" {:v {:arr count_inversions_recursive_arr :inv 0}}))) (set! count_inversions_recursive_mid (quot (count count_inversions_recursive_arr) 2)) (set! count_inversions_recursive_p (slice_list count_inversions_recursive_arr 0 count_inversions_recursive_mid)) (set! count_inversions_recursive_q (slice_list count_inversions_recursive_arr count_inversions_recursive_mid (count count_inversions_recursive_arr))) (set! count_inversions_recursive_res_p (count_inversions_recursive count_inversions_recursive_p)) (set! count_inversions_recursive_res_q (count_inversions_recursive count_inversions_recursive_q)) (set! count_inversions_recursive_res_cross (count_cross_inversions (:arr count_inversions_recursive_res_p) (:arr count_inversions_recursive_res_q))) (set! count_inversions_recursive_total (+ (+ (:inv count_inversions_recursive_res_p) (:inv count_inversions_recursive_res_q)) (:inv count_inversions_recursive_res_cross))) (throw (ex-info "return" {:v {:arr (:arr count_inversions_recursive_res_cross) :inv count_inversions_recursive_total}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_arr_1 [10 2 1 5 5 2 11])

(def ^:dynamic main_nbf (count_inversions_bf main_arr_1))

(def ^:dynamic main_nrec (:inv (count_inversions_recursive main_arr_1)))

(def ^:dynamic main_nbf2 (count_inversions_bf main_arr_1))

(def ^:dynamic main_nrec2 (:inv (count_inversions_recursive main_arr_1)))

(def ^:dynamic main_nbf3 (count_inversions_bf main_arr_1))

(def ^:dynamic main_nrec3 (:inv (count_inversions_recursive main_arr_1)))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "number of inversions = " main_nbf)
      (def main_arr_1 [1 2 2 5 5 10 11])
      (println "number of inversions = " main_nbf2)
      (def main_arr_1 [])
      (println "number of inversions = " main_nbf3)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
