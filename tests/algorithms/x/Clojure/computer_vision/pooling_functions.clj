(ns main (:refer-clojure :exclude [maxpooling avgpooling print_matrix main]))

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

(declare maxpooling avgpooling print_matrix main)

(def ^:dynamic avgpooling_c nil)

(def ^:dynamic avgpooling_i nil)

(def ^:dynamic avgpooling_j nil)

(def ^:dynamic avgpooling_n nil)

(def ^:dynamic avgpooling_r nil)

(def ^:dynamic avgpooling_result nil)

(def ^:dynamic avgpooling_row nil)

(def ^:dynamic avgpooling_sum nil)

(def ^:dynamic main_arr1 nil)

(def ^:dynamic main_arr2 nil)

(def ^:dynamic maxpooling_c nil)

(def ^:dynamic maxpooling_i nil)

(def ^:dynamic maxpooling_j nil)

(def ^:dynamic maxpooling_max_val nil)

(def ^:dynamic maxpooling_n nil)

(def ^:dynamic maxpooling_r nil)

(def ^:dynamic maxpooling_result nil)

(def ^:dynamic maxpooling_row nil)

(def ^:dynamic maxpooling_val nil)

(def ^:dynamic print_matrix_i nil)

(def ^:dynamic print_matrix_j nil)

(def ^:dynamic print_matrix_line nil)

(defn maxpooling [maxpooling_arr maxpooling_size maxpooling_stride]
  (binding [maxpooling_c nil maxpooling_i nil maxpooling_j nil maxpooling_max_val nil maxpooling_n nil maxpooling_r nil maxpooling_result nil maxpooling_row nil maxpooling_val nil] (try (do (set! maxpooling_n (count maxpooling_arr)) (when (or (= maxpooling_n 0) (not= (count (nth maxpooling_arr 0)) maxpooling_n)) (throw (Exception. "The input array is not a square matrix"))) (set! maxpooling_result []) (set! maxpooling_i 0) (while (<= (+ maxpooling_i maxpooling_size) maxpooling_n) (do (set! maxpooling_row []) (set! maxpooling_j 0) (while (<= (+ maxpooling_j maxpooling_size) maxpooling_n) (do (set! maxpooling_max_val (nth (nth maxpooling_arr maxpooling_i) maxpooling_j)) (set! maxpooling_r maxpooling_i) (while (< maxpooling_r (+ maxpooling_i maxpooling_size)) (do (set! maxpooling_c maxpooling_j) (while (< maxpooling_c (+ maxpooling_j maxpooling_size)) (do (set! maxpooling_val (nth (nth maxpooling_arr maxpooling_r) maxpooling_c)) (when (> maxpooling_val maxpooling_max_val) (set! maxpooling_max_val maxpooling_val)) (set! maxpooling_c (+ maxpooling_c 1)))) (set! maxpooling_r (+ maxpooling_r 1)))) (set! maxpooling_row (conj maxpooling_row maxpooling_max_val)) (set! maxpooling_j (+ maxpooling_j maxpooling_stride)))) (set! maxpooling_result (conj maxpooling_result maxpooling_row)) (set! maxpooling_i (+ maxpooling_i maxpooling_stride)))) (throw (ex-info "return" {:v maxpooling_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn avgpooling [avgpooling_arr avgpooling_size avgpooling_stride]
  (binding [avgpooling_c nil avgpooling_i nil avgpooling_j nil avgpooling_n nil avgpooling_r nil avgpooling_result nil avgpooling_row nil avgpooling_sum nil] (try (do (set! avgpooling_n (count avgpooling_arr)) (when (or (= avgpooling_n 0) (not= (count (nth avgpooling_arr 0)) avgpooling_n)) (throw (Exception. "The input array is not a square matrix"))) (set! avgpooling_result []) (set! avgpooling_i 0) (while (<= (+ avgpooling_i avgpooling_size) avgpooling_n) (do (set! avgpooling_row []) (set! avgpooling_j 0) (while (<= (+ avgpooling_j avgpooling_size) avgpooling_n) (do (set! avgpooling_sum 0) (set! avgpooling_r avgpooling_i) (while (< avgpooling_r (+ avgpooling_i avgpooling_size)) (do (set! avgpooling_c avgpooling_j) (while (< avgpooling_c (+ avgpooling_j avgpooling_size)) (do (set! avgpooling_sum (+ avgpooling_sum (nth (nth avgpooling_arr avgpooling_r) avgpooling_c))) (set! avgpooling_c (+ avgpooling_c 1)))) (set! avgpooling_r (+ avgpooling_r 1)))) (set! avgpooling_row (conj avgpooling_row (/ avgpooling_sum (* avgpooling_size avgpooling_size)))) (set! avgpooling_j (+ avgpooling_j avgpooling_stride)))) (set! avgpooling_result (conj avgpooling_result avgpooling_row)) (set! avgpooling_i (+ avgpooling_i avgpooling_stride)))) (throw (ex-info "return" {:v avgpooling_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_matrix [print_matrix_mat]
  (binding [print_matrix_i nil print_matrix_j nil print_matrix_line nil] (do (set! print_matrix_i 0) (while (< print_matrix_i (count print_matrix_mat)) (do (set! print_matrix_line "") (set! print_matrix_j 0) (while (< print_matrix_j (count (nth print_matrix_mat print_matrix_i))) (do (set! print_matrix_line (str print_matrix_line (str (nth (nth print_matrix_mat print_matrix_i) print_matrix_j)))) (when (< print_matrix_j (- (count (nth print_matrix_mat print_matrix_i)) 1)) (set! print_matrix_line (str print_matrix_line " "))) (set! print_matrix_j (+ print_matrix_j 1)))) (println print_matrix_line) (set! print_matrix_i (+ print_matrix_i 1)))))))

(defn main []
  (binding [main_arr1 nil main_arr2 nil] (do (set! main_arr1 [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 14 15 16]]) (set! main_arr2 [[147 180 122] [241 76 32] [126 13 157]]) (print_matrix (maxpooling main_arr1 2 2)) (print_matrix (maxpooling main_arr2 2 1)) (print_matrix (avgpooling main_arr1 2 2)) (print_matrix (avgpooling main_arr2 2 1)))))

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
