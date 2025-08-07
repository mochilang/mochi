(ns main (:refer-clojure :exclude [get_neighbors_pixel local_binary_value]))

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

(declare get_neighbors_pixel local_binary_value)

(def ^:dynamic local_binary_value_center nil)

(def ^:dynamic local_binary_value_i nil)

(def ^:dynamic local_binary_value_neighbors nil)

(def ^:dynamic local_binary_value_powers nil)

(def ^:dynamic local_binary_value_sum nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_j nil)

(def ^:dynamic main_line nil)

(defn get_neighbors_pixel [get_neighbors_pixel_image get_neighbors_pixel_x get_neighbors_pixel_y get_neighbors_pixel_center]
  (try (do (when (or (< get_neighbors_pixel_x 0) (< get_neighbors_pixel_y 0)) (throw (ex-info "return" {:v 0}))) (when (or (>= get_neighbors_pixel_x (count get_neighbors_pixel_image)) (>= get_neighbors_pixel_y (count (nth get_neighbors_pixel_image 0)))) (throw (ex-info "return" {:v 0}))) (if (>= (nth (nth get_neighbors_pixel_image get_neighbors_pixel_x) get_neighbors_pixel_y) get_neighbors_pixel_center) 1 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn local_binary_value [local_binary_value_image local_binary_value_x local_binary_value_y]
  (binding [local_binary_value_center nil local_binary_value_i nil local_binary_value_neighbors nil local_binary_value_powers nil local_binary_value_sum nil] (try (do (set! local_binary_value_center (nth (nth local_binary_value_image local_binary_value_x) local_binary_value_y)) (set! local_binary_value_powers [1 2 4 8 16 32 64 128]) (set! local_binary_value_neighbors [(get_neighbors_pixel local_binary_value_image (- local_binary_value_x 1) (+ local_binary_value_y 1) local_binary_value_center) (get_neighbors_pixel local_binary_value_image local_binary_value_x (+ local_binary_value_y 1) local_binary_value_center) (get_neighbors_pixel local_binary_value_image (- local_binary_value_x 1) local_binary_value_y local_binary_value_center) (get_neighbors_pixel local_binary_value_image (+ local_binary_value_x 1) (+ local_binary_value_y 1) local_binary_value_center) (get_neighbors_pixel local_binary_value_image (+ local_binary_value_x 1) local_binary_value_y local_binary_value_center) (get_neighbors_pixel local_binary_value_image (+ local_binary_value_x 1) (- local_binary_value_y 1) local_binary_value_center) (get_neighbors_pixel local_binary_value_image local_binary_value_x (- local_binary_value_y 1) local_binary_value_center) (get_neighbors_pixel local_binary_value_image (- local_binary_value_x 1) (- local_binary_value_y 1) local_binary_value_center)]) (set! local_binary_value_sum 0) (set! local_binary_value_i 0) (while (< local_binary_value_i (count local_binary_value_neighbors)) (do (set! local_binary_value_sum (+ local_binary_value_sum (* (nth local_binary_value_neighbors local_binary_value_i) (nth local_binary_value_powers local_binary_value_i)))) (set! local_binary_value_i (+ local_binary_value_i 1)))) (throw (ex-info "return" {:v local_binary_value_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_image [[10 10 10 10 10] [10 20 30 20 10] [10 30 40 30 10] [10 20 30 20 10] [10 10 10 10 10]])

(def ^:dynamic main_i 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_i (count main_image)) (do (def ^:dynamic main_j 0) (def ^:dynamic main_line "") (while (< main_j (count (nth main_image 0))) (do (def ^:dynamic main_value (local_binary_value main_image main_i main_j)) (def main_line (str main_line (str main_value))) (when (< main_j (- (count (nth main_image 0)) 1)) (def main_line (str main_line " "))) (def main_j (+ main_j 1)))) (println main_line) (def main_i (+ main_i 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
