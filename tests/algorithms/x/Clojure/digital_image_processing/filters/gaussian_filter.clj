(ns main (:refer-clojure :exclude [expApprox gen_gaussian_kernel gaussian_filter print_image]))

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

(declare expApprox gen_gaussian_kernel gaussian_filter print_image)

(def ^:dynamic expApprox_n nil)

(def ^:dynamic expApprox_sum nil)

(def ^:dynamic expApprox_term nil)

(def ^:dynamic gaussian_filter_dst nil)

(def ^:dynamic gaussian_filter_dst_height nil)

(def ^:dynamic gaussian_filter_dst_width nil)

(def ^:dynamic gaussian_filter_height nil)

(def ^:dynamic gaussian_filter_i nil)

(def ^:dynamic gaussian_filter_j nil)

(def ^:dynamic gaussian_filter_kernel nil)

(def ^:dynamic gaussian_filter_ki nil)

(def ^:dynamic gaussian_filter_kj nil)

(def ^:dynamic gaussian_filter_row nil)

(def ^:dynamic gaussian_filter_sum nil)

(def ^:dynamic gaussian_filter_width nil)

(def ^:dynamic gen_gaussian_kernel_center nil)

(def ^:dynamic gen_gaussian_kernel_exponent nil)

(def ^:dynamic gen_gaussian_kernel_i nil)

(def ^:dynamic gen_gaussian_kernel_j nil)

(def ^:dynamic gen_gaussian_kernel_kernel nil)

(def ^:dynamic gen_gaussian_kernel_row nil)

(def ^:dynamic gen_gaussian_kernel_value nil)

(def ^:dynamic gen_gaussian_kernel_x nil)

(def ^:dynamic gen_gaussian_kernel_y nil)

(def ^:dynamic print_image_i nil)

(def ^:dynamic main_PI 3.141592653589793)

(defn expApprox [expApprox_x]
  (binding [expApprox_n nil expApprox_sum nil expApprox_term nil] (try (do (set! expApprox_sum 1.0) (set! expApprox_term 1.0) (set! expApprox_n 1) (while (< expApprox_n 10) (do (set! expApprox_term (quot (* expApprox_term expApprox_x) (double expApprox_n))) (set! expApprox_sum (+ expApprox_sum expApprox_term)) (set! expApprox_n (+ expApprox_n 1)))) (throw (ex-info "return" {:v expApprox_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn gen_gaussian_kernel [gen_gaussian_kernel_k_size gen_gaussian_kernel_sigma]
  (binding [gen_gaussian_kernel_center nil gen_gaussian_kernel_exponent nil gen_gaussian_kernel_i nil gen_gaussian_kernel_j nil gen_gaussian_kernel_kernel nil gen_gaussian_kernel_row nil gen_gaussian_kernel_value nil gen_gaussian_kernel_x nil gen_gaussian_kernel_y nil] (try (do (set! gen_gaussian_kernel_center (quot gen_gaussian_kernel_k_size 2)) (set! gen_gaussian_kernel_kernel []) (set! gen_gaussian_kernel_i 0) (while (< gen_gaussian_kernel_i gen_gaussian_kernel_k_size) (do (set! gen_gaussian_kernel_row []) (set! gen_gaussian_kernel_j 0) (while (< gen_gaussian_kernel_j gen_gaussian_kernel_k_size) (do (set! gen_gaussian_kernel_x (double (- gen_gaussian_kernel_i gen_gaussian_kernel_center))) (set! gen_gaussian_kernel_y (double (- gen_gaussian_kernel_j gen_gaussian_kernel_center))) (set! gen_gaussian_kernel_exponent (- (quot (+ (* gen_gaussian_kernel_x gen_gaussian_kernel_x) (* gen_gaussian_kernel_y gen_gaussian_kernel_y)) (* (* 2.0 gen_gaussian_kernel_sigma) gen_gaussian_kernel_sigma)))) (set! gen_gaussian_kernel_value (* (/ 1.0 (* (* 2.0 main_PI) gen_gaussian_kernel_sigma)) (expApprox gen_gaussian_kernel_exponent))) (set! gen_gaussian_kernel_row (conj gen_gaussian_kernel_row gen_gaussian_kernel_value)) (set! gen_gaussian_kernel_j (+ gen_gaussian_kernel_j 1)))) (set! gen_gaussian_kernel_kernel (conj gen_gaussian_kernel_kernel gen_gaussian_kernel_row)) (set! gen_gaussian_kernel_i (+ gen_gaussian_kernel_i 1)))) (throw (ex-info "return" {:v gen_gaussian_kernel_kernel}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn gaussian_filter [gaussian_filter_image gaussian_filter_k_size gaussian_filter_sigma]
  (binding [gaussian_filter_dst nil gaussian_filter_dst_height nil gaussian_filter_dst_width nil gaussian_filter_height nil gaussian_filter_i nil gaussian_filter_j nil gaussian_filter_kernel nil gaussian_filter_ki nil gaussian_filter_kj nil gaussian_filter_row nil gaussian_filter_sum nil gaussian_filter_width nil] (try (do (set! gaussian_filter_height (count gaussian_filter_image)) (set! gaussian_filter_width (count (nth gaussian_filter_image 0))) (set! gaussian_filter_dst_height (+ (- gaussian_filter_height gaussian_filter_k_size) 1)) (set! gaussian_filter_dst_width (+ (- gaussian_filter_width gaussian_filter_k_size) 1)) (set! gaussian_filter_kernel (gen_gaussian_kernel gaussian_filter_k_size gaussian_filter_sigma)) (set! gaussian_filter_dst []) (set! gaussian_filter_i 0) (while (< gaussian_filter_i gaussian_filter_dst_height) (do (set! gaussian_filter_row []) (set! gaussian_filter_j 0) (while (< gaussian_filter_j gaussian_filter_dst_width) (do (set! gaussian_filter_sum 0.0) (set! gaussian_filter_ki 0) (while (< gaussian_filter_ki gaussian_filter_k_size) (do (set! gaussian_filter_kj 0) (while (< gaussian_filter_kj gaussian_filter_k_size) (do (set! gaussian_filter_sum (+ gaussian_filter_sum (* (double (nth (nth gaussian_filter_image (+ gaussian_filter_i gaussian_filter_ki)) (+ gaussian_filter_j gaussian_filter_kj))) (nth (nth gaussian_filter_kernel gaussian_filter_ki) gaussian_filter_kj)))) (set! gaussian_filter_kj (+ gaussian_filter_kj 1)))) (set! gaussian_filter_ki (+ gaussian_filter_ki 1)))) (set! gaussian_filter_row (conj gaussian_filter_row (long gaussian_filter_sum))) (set! gaussian_filter_j (+ gaussian_filter_j 1)))) (set! gaussian_filter_dst (conj gaussian_filter_dst gaussian_filter_row)) (set! gaussian_filter_i (+ gaussian_filter_i 1)))) (throw (ex-info "return" {:v gaussian_filter_dst}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_image [print_image_image]
  (binding [print_image_i nil] (do (set! print_image_i 0) (while (< print_image_i (count print_image_image)) (do (println (nth print_image_image print_image_i)) (set! print_image_i (+ print_image_i 1)))))))

(def ^:dynamic main_img [[52 55 61 59 79] [62 59 55 104 94] [63 65 66 113 144] [68 70 70 126 154] [70 72 69 128 155]])

(def ^:dynamic main_gaussian3 (gaussian_filter main_img 3 1.0))

(def ^:dynamic main_gaussian5 (gaussian_filter main_img 5 0.8))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (print_image main_gaussian3)
      (print_image main_gaussian5)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
