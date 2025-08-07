(ns main (:refer-clojure :exclude [flip_horizontal_image flip_vertical_image flip_horizontal_boxes flip_vertical_boxes print_image]))

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

(declare flip_horizontal_image flip_vertical_image flip_horizontal_boxes flip_vertical_boxes print_image)

(def ^:dynamic flip_horizontal_boxes_b nil)

(def ^:dynamic flip_horizontal_boxes_i nil)

(def ^:dynamic flip_horizontal_boxes_result nil)

(def ^:dynamic flip_horizontal_boxes_x_new nil)

(def ^:dynamic flip_horizontal_image_flipped nil)

(def ^:dynamic flip_horizontal_image_i nil)

(def ^:dynamic flip_horizontal_image_j nil)

(def ^:dynamic flip_horizontal_image_new_row nil)

(def ^:dynamic flip_horizontal_image_row nil)

(def ^:dynamic flip_vertical_boxes_b nil)

(def ^:dynamic flip_vertical_boxes_i nil)

(def ^:dynamic flip_vertical_boxes_result nil)

(def ^:dynamic flip_vertical_boxes_y_new nil)

(def ^:dynamic flip_vertical_image_flipped nil)

(def ^:dynamic flip_vertical_image_i nil)

(def ^:dynamic print_image_i nil)

(def ^:dynamic print_image_j nil)

(def ^:dynamic print_image_line nil)

(def ^:dynamic print_image_row nil)

(defn flip_horizontal_image [flip_horizontal_image_img]
  (binding [flip_horizontal_image_flipped nil flip_horizontal_image_i nil flip_horizontal_image_j nil flip_horizontal_image_new_row nil flip_horizontal_image_row nil] (try (do (set! flip_horizontal_image_flipped []) (set! flip_horizontal_image_i 0) (while (< flip_horizontal_image_i (count flip_horizontal_image_img)) (do (set! flip_horizontal_image_row (nth flip_horizontal_image_img flip_horizontal_image_i)) (set! flip_horizontal_image_j (- (count flip_horizontal_image_row) 1)) (set! flip_horizontal_image_new_row []) (while (>= flip_horizontal_image_j 0) (do (set! flip_horizontal_image_new_row (conj flip_horizontal_image_new_row (nth flip_horizontal_image_row flip_horizontal_image_j))) (set! flip_horizontal_image_j (- flip_horizontal_image_j 1)))) (set! flip_horizontal_image_flipped (conj flip_horizontal_image_flipped flip_horizontal_image_new_row)) (set! flip_horizontal_image_i (+ flip_horizontal_image_i 1)))) (throw (ex-info "return" {:v flip_horizontal_image_flipped}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn flip_vertical_image [flip_vertical_image_img]
  (binding [flip_vertical_image_flipped nil flip_vertical_image_i nil] (try (do (set! flip_vertical_image_flipped []) (set! flip_vertical_image_i (- (count flip_vertical_image_img) 1)) (while (>= flip_vertical_image_i 0) (do (set! flip_vertical_image_flipped (conj flip_vertical_image_flipped (nth flip_vertical_image_img flip_vertical_image_i))) (set! flip_vertical_image_i (- flip_vertical_image_i 1)))) (throw (ex-info "return" {:v flip_vertical_image_flipped}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn flip_horizontal_boxes [flip_horizontal_boxes_boxes]
  (binding [flip_horizontal_boxes_b nil flip_horizontal_boxes_i nil flip_horizontal_boxes_result nil flip_horizontal_boxes_x_new nil] (try (do (set! flip_horizontal_boxes_result []) (set! flip_horizontal_boxes_i 0) (while (< flip_horizontal_boxes_i (count flip_horizontal_boxes_boxes)) (do (set! flip_horizontal_boxes_b (nth flip_horizontal_boxes_boxes flip_horizontal_boxes_i)) (set! flip_horizontal_boxes_x_new (- 1.0 (nth flip_horizontal_boxes_b 1))) (set! flip_horizontal_boxes_result (conj flip_horizontal_boxes_result [(nth flip_horizontal_boxes_b 0) flip_horizontal_boxes_x_new (nth flip_horizontal_boxes_b 2) (nth flip_horizontal_boxes_b 3) (nth flip_horizontal_boxes_b 4)])) (set! flip_horizontal_boxes_i (+ flip_horizontal_boxes_i 1)))) (throw (ex-info "return" {:v flip_horizontal_boxes_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn flip_vertical_boxes [flip_vertical_boxes_boxes]
  (binding [flip_vertical_boxes_b nil flip_vertical_boxes_i nil flip_vertical_boxes_result nil flip_vertical_boxes_y_new nil] (try (do (set! flip_vertical_boxes_result []) (set! flip_vertical_boxes_i 0) (while (< flip_vertical_boxes_i (count flip_vertical_boxes_boxes)) (do (set! flip_vertical_boxes_b (nth flip_vertical_boxes_boxes flip_vertical_boxes_i)) (set! flip_vertical_boxes_y_new (- 1.0 (nth flip_vertical_boxes_b 2))) (set! flip_vertical_boxes_result (conj flip_vertical_boxes_result [(nth flip_vertical_boxes_b 0) (nth flip_vertical_boxes_b 1) flip_vertical_boxes_y_new (nth flip_vertical_boxes_b 3) (nth flip_vertical_boxes_b 4)])) (set! flip_vertical_boxes_i (+ flip_vertical_boxes_i 1)))) (throw (ex-info "return" {:v flip_vertical_boxes_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_image [print_image_img]
  (binding [print_image_i nil print_image_j nil print_image_line nil print_image_row nil] (do (set! print_image_i 0) (while (< print_image_i (count print_image_img)) (do (set! print_image_row (nth print_image_img print_image_i)) (set! print_image_j 0) (set! print_image_line "") (while (< print_image_j (count print_image_row)) (do (set! print_image_line (str (str print_image_line (str (nth print_image_row print_image_j))) " ")) (set! print_image_j (+ print_image_j 1)))) (println print_image_line) (set! print_image_i (+ print_image_i 1)))))))

(def ^:dynamic main_image [[1 2 3] [4 5 6] [7 8 9]])

(def ^:dynamic main_boxes [[0.0 0.25 0.25 0.5 0.5] [1.0 0.75 0.75 0.5 0.5]])

(def ^:dynamic main_h_img (flip_horizontal_image main_image))

(def ^:dynamic main_h_boxes (flip_horizontal_boxes main_boxes))

(def ^:dynamic main_v_img (flip_vertical_image main_image))

(def ^:dynamic main_v_boxes (flip_vertical_boxes main_boxes))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "Original image:")
      (print_image main_image)
      (println (str main_boxes))
      (println "Horizontal flip:")
      (print_image main_h_img)
      (println (str main_h_boxes))
      (println "Vertical flip:")
      (print_image main_v_img)
      (println (str main_v_boxes))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
