(ns main (:refer-clojure :exclude [zeros3d resize_nn main]))

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

(declare zeros3d resize_nn main)

(def ^:dynamic main_img nil)

(def ^:dynamic main_resized nil)

(def ^:dynamic resize_nn_channels nil)

(def ^:dynamic resize_nn_i nil)

(def ^:dynamic resize_nn_j nil)

(def ^:dynamic resize_nn_out nil)

(def ^:dynamic resize_nn_ratio_x nil)

(def ^:dynamic resize_nn_ratio_y nil)

(def ^:dynamic resize_nn_src_h nil)

(def ^:dynamic resize_nn_src_w nil)

(def ^:dynamic resize_nn_src_x nil)

(def ^:dynamic resize_nn_src_y nil)

(def ^:dynamic zeros3d_arr nil)

(def ^:dynamic zeros3d_k nil)

(def ^:dynamic zeros3d_pixel nil)

(def ^:dynamic zeros3d_row nil)

(def ^:dynamic zeros3d_x nil)

(def ^:dynamic zeros3d_y nil)

(defn zeros3d [zeros3d_h zeros3d_w zeros3d_c]
  (binding [zeros3d_arr nil zeros3d_k nil zeros3d_pixel nil zeros3d_row nil zeros3d_x nil zeros3d_y nil] (try (do (set! zeros3d_arr []) (set! zeros3d_y 0) (while (< zeros3d_y zeros3d_h) (do (set! zeros3d_row []) (set! zeros3d_x 0) (while (< zeros3d_x zeros3d_w) (do (set! zeros3d_pixel []) (set! zeros3d_k 0) (while (< zeros3d_k zeros3d_c) (do (set! zeros3d_pixel (conj zeros3d_pixel 0)) (set! zeros3d_k (+ zeros3d_k 1)))) (set! zeros3d_row (conj zeros3d_row zeros3d_pixel)) (set! zeros3d_x (+ zeros3d_x 1)))) (set! zeros3d_arr (conj zeros3d_arr zeros3d_row)) (set! zeros3d_y (+ zeros3d_y 1)))) (throw (ex-info "return" {:v zeros3d_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn resize_nn [resize_nn_img resize_nn_dst_w resize_nn_dst_h]
  (binding [resize_nn_channels nil resize_nn_i nil resize_nn_j nil resize_nn_out nil resize_nn_ratio_x nil resize_nn_ratio_y nil resize_nn_src_h nil resize_nn_src_w nil resize_nn_src_x nil resize_nn_src_y nil] (try (do (set! resize_nn_src_h (count resize_nn_img)) (set! resize_nn_src_w (count (nth resize_nn_img 0))) (set! resize_nn_channels (count (nth (nth resize_nn_img 0) 0))) (set! resize_nn_ratio_x (quot (double resize_nn_src_w) (double resize_nn_dst_w))) (set! resize_nn_ratio_y (quot (double resize_nn_src_h) (double resize_nn_dst_h))) (set! resize_nn_out (zeros3d resize_nn_dst_h resize_nn_dst_w resize_nn_channels)) (set! resize_nn_i 0) (while (< resize_nn_i resize_nn_dst_h) (do (set! resize_nn_j 0) (while (< resize_nn_j resize_nn_dst_w) (do (set! resize_nn_src_x (long (* resize_nn_ratio_x (double resize_nn_j)))) (set! resize_nn_src_y (long (* resize_nn_ratio_y (double resize_nn_i)))) (set! resize_nn_out (assoc-in resize_nn_out [resize_nn_i resize_nn_j] (nth (nth resize_nn_img resize_nn_src_y) resize_nn_src_x))) (set! resize_nn_j (+ resize_nn_j 1)))) (set! resize_nn_i (+ resize_nn_i 1)))) (throw (ex-info "return" {:v resize_nn_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_img nil main_resized nil] (do (set! main_img [[[0 0 0] [255 255 255]] [[255 0 0] [0 255 0]]]) (set! main_resized (resize_nn main_img 4 4)) (println main_resized))))

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
