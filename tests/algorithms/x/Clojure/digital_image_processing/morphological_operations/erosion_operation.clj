(ns main (:refer-clojure :exclude [rgb_to_gray gray_to_binary erosion]))

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

(declare rgb_to_gray gray_to_binary erosion)

(def ^:dynamic erosion_h nil)

(def ^:dynamic erosion_k_h nil)

(def ^:dynamic erosion_k_w nil)

(def ^:dynamic erosion_kx nil)

(def ^:dynamic erosion_ky nil)

(def ^:dynamic erosion_output nil)

(def ^:dynamic erosion_pad_x nil)

(def ^:dynamic erosion_pad_y nil)

(def ^:dynamic erosion_padded nil)

(def ^:dynamic erosion_row nil)

(def ^:dynamic erosion_row_out nil)

(def ^:dynamic erosion_sum nil)

(def ^:dynamic erosion_w nil)

(def ^:dynamic erosion_x nil)

(def ^:dynamic erosion_y nil)

(def ^:dynamic gray_to_binary_binary nil)

(def ^:dynamic gray_to_binary_i nil)

(def ^:dynamic gray_to_binary_j nil)

(def ^:dynamic gray_to_binary_row nil)

(def ^:dynamic rgb_to_gray_b nil)

(def ^:dynamic rgb_to_gray_g nil)

(def ^:dynamic rgb_to_gray_gray nil)

(def ^:dynamic rgb_to_gray_i nil)

(def ^:dynamic rgb_to_gray_j nil)

(def ^:dynamic rgb_to_gray_r nil)

(def ^:dynamic rgb_to_gray_row nil)

(def ^:dynamic rgb_to_gray_value nil)

(defn rgb_to_gray [rgb_to_gray_rgb]
  (binding [rgb_to_gray_b nil rgb_to_gray_g nil rgb_to_gray_gray nil rgb_to_gray_i nil rgb_to_gray_j nil rgb_to_gray_r nil rgb_to_gray_row nil rgb_to_gray_value nil] (try (do (set! rgb_to_gray_gray []) (set! rgb_to_gray_i 0) (while (< rgb_to_gray_i (count rgb_to_gray_rgb)) (do (set! rgb_to_gray_row []) (set! rgb_to_gray_j 0) (while (< rgb_to_gray_j (count (nth rgb_to_gray_rgb rgb_to_gray_i))) (do (set! rgb_to_gray_r (double (nth (nth (nth rgb_to_gray_rgb rgb_to_gray_i) rgb_to_gray_j) 0))) (set! rgb_to_gray_g (double (nth (nth (nth rgb_to_gray_rgb rgb_to_gray_i) rgb_to_gray_j) 1))) (set! rgb_to_gray_b (double (nth (nth (nth rgb_to_gray_rgb rgb_to_gray_i) rgb_to_gray_j) 2))) (set! rgb_to_gray_value (+ (+ (* 0.2989 rgb_to_gray_r) (* 0.587 rgb_to_gray_g)) (* 0.114 rgb_to_gray_b))) (set! rgb_to_gray_row (conj rgb_to_gray_row rgb_to_gray_value)) (set! rgb_to_gray_j (+ rgb_to_gray_j 1)))) (set! rgb_to_gray_gray (conj rgb_to_gray_gray rgb_to_gray_row)) (set! rgb_to_gray_i (+ rgb_to_gray_i 1)))) (throw (ex-info "return" {:v rgb_to_gray_gray}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn gray_to_binary [gray_to_binary_gray]
  (binding [gray_to_binary_binary nil gray_to_binary_i nil gray_to_binary_j nil gray_to_binary_row nil] (try (do (set! gray_to_binary_binary []) (set! gray_to_binary_i 0) (while (< gray_to_binary_i (count gray_to_binary_gray)) (do (set! gray_to_binary_row []) (set! gray_to_binary_j 0) (while (< gray_to_binary_j (count (nth gray_to_binary_gray gray_to_binary_i))) (do (set! gray_to_binary_row (conj gray_to_binary_row (and (> (nth (nth gray_to_binary_gray gray_to_binary_i) gray_to_binary_j) 127.0) (<= (nth (nth gray_to_binary_gray gray_to_binary_i) gray_to_binary_j) 255.0)))) (set! gray_to_binary_j (+ gray_to_binary_j 1)))) (set! gray_to_binary_binary (conj gray_to_binary_binary gray_to_binary_row)) (set! gray_to_binary_i (+ gray_to_binary_i 1)))) (throw (ex-info "return" {:v gray_to_binary_binary}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn erosion [erosion_image erosion_kernel]
  (binding [erosion_h nil erosion_k_h nil erosion_k_w nil erosion_kx nil erosion_ky nil erosion_output nil erosion_pad_x nil erosion_pad_y nil erosion_padded nil erosion_row nil erosion_row_out nil erosion_sum nil erosion_w nil erosion_x nil erosion_y nil] (try (do (set! erosion_h (count erosion_image)) (set! erosion_w (count (nth erosion_image 0))) (set! erosion_k_h (count erosion_kernel)) (set! erosion_k_w (count (nth erosion_kernel 0))) (set! erosion_pad_y (quot erosion_k_h 2)) (set! erosion_pad_x (quot erosion_k_w 2)) (set! erosion_padded []) (set! erosion_y 0) (while (< erosion_y (+ erosion_h (* 2 erosion_pad_y))) (do (set! erosion_row []) (set! erosion_x 0) (while (< erosion_x (+ erosion_w (* 2 erosion_pad_x))) (do (set! erosion_row (conj erosion_row false)) (set! erosion_x (+ erosion_x 1)))) (set! erosion_padded (conj erosion_padded erosion_row)) (set! erosion_y (+ erosion_y 1)))) (set! erosion_y 0) (while (< erosion_y erosion_h) (do (set! erosion_x 0) (while (< erosion_x erosion_w) (do (set! erosion_padded (assoc-in erosion_padded [(+ erosion_pad_y erosion_y) (+ erosion_pad_x erosion_x)] (nth (nth erosion_image erosion_y) erosion_x))) (set! erosion_x (+ erosion_x 1)))) (set! erosion_y (+ erosion_y 1)))) (set! erosion_output []) (set! erosion_y 0) (while (< erosion_y erosion_h) (do (set! erosion_row_out []) (set! erosion_x 0) (while (< erosion_x erosion_w) (do (set! erosion_sum 0) (set! erosion_ky 0) (while (< erosion_ky erosion_k_h) (do (set! erosion_kx 0) (while (< erosion_kx erosion_k_w) (do (when (and (= (nth (nth erosion_kernel erosion_ky) erosion_kx) 1) (nth (nth erosion_padded (+ erosion_y erosion_ky)) (+ erosion_x erosion_kx))) (set! erosion_sum (+ erosion_sum 1))) (set! erosion_kx (+ erosion_kx 1)))) (set! erosion_ky (+ erosion_ky 1)))) (set! erosion_row_out (conj erosion_row_out (= erosion_sum 5))) (set! erosion_x (+ erosion_x 1)))) (set! erosion_output (conj erosion_output erosion_row_out)) (set! erosion_y (+ erosion_y 1)))) (throw (ex-info "return" {:v erosion_output}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_rgb_img [[[127 255 0]]])

(def ^:dynamic main_gray_img [[127.0 255.0 0.0]])

(def ^:dynamic main_img1 [[true true false]])

(def ^:dynamic main_kernel1 [[0 1 0]])

(def ^:dynamic main_img2 [[true false false]])

(def ^:dynamic main_kernel2 [[1 1 0]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (rgb_to_gray main_rgb_img)))
      (println (str (gray_to_binary main_gray_img)))
      (println (str (erosion main_img1 main_kernel1)))
      (println (str (erosion main_img2 main_kernel2)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
