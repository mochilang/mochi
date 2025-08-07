(ns main (:refer-clojure :exclude [update_image_and_anno main]))

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

(declare update_image_and_anno main)

(def ^:dynamic main_a nil)

(def ^:dynamic main_all_annos nil)

(def ^:dynamic main_all_img_list nil)

(def ^:dynamic main_filter_scale nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_idxs nil)

(def ^:dynamic main_new_annos nil)

(def ^:dynamic main_output_size nil)

(def ^:dynamic main_path nil)

(def ^:dynamic main_res nil)

(def ^:dynamic main_scale_range nil)

(def ^:dynamic update_image_and_anno_anno nil)

(def ^:dynamic update_image_and_anno_bbox nil)

(def ^:dynamic update_image_and_anno_bbox1 nil)

(def ^:dynamic update_image_and_anno_bbox2 nil)

(def ^:dynamic update_image_and_anno_bbox3 nil)

(def ^:dynamic update_image_and_anno_c nil)

(def ^:dynamic update_image_and_anno_divid_point_x nil)

(def ^:dynamic update_image_and_anno_divid_point_y nil)

(def ^:dynamic update_image_and_anno_filtered nil)

(def ^:dynamic update_image_and_anno_h nil)

(def ^:dynamic update_image_and_anno_height nil)

(def ^:dynamic update_image_and_anno_i nil)

(def ^:dynamic update_image_and_anno_img_annos nil)

(def ^:dynamic update_image_and_anno_index nil)

(def ^:dynamic update_image_and_anno_j0 nil)

(def ^:dynamic update_image_and_anno_j1 nil)

(def ^:dynamic update_image_and_anno_j2 nil)

(def ^:dynamic update_image_and_anno_j3 nil)

(def ^:dynamic update_image_and_anno_k nil)

(def ^:dynamic update_image_and_anno_new_anno nil)

(def ^:dynamic update_image_and_anno_output_img nil)

(def ^:dynamic update_image_and_anno_path nil)

(def ^:dynamic update_image_and_anno_path_list nil)

(def ^:dynamic update_image_and_anno_r nil)

(def ^:dynamic update_image_and_anno_row nil)

(def ^:dynamic update_image_and_anno_scale_x nil)

(def ^:dynamic update_image_and_anno_scale_y nil)

(def ^:dynamic update_image_and_anno_w nil)

(def ^:dynamic update_image_and_anno_width nil)

(def ^:dynamic update_image_and_anno_x0 nil)

(def ^:dynamic update_image_and_anno_x1 nil)

(def ^:dynamic update_image_and_anno_x2 nil)

(def ^:dynamic update_image_and_anno_x3 nil)

(def ^:dynamic update_image_and_anno_xmax nil)

(def ^:dynamic update_image_and_anno_xmax1 nil)

(def ^:dynamic update_image_and_anno_xmax2 nil)

(def ^:dynamic update_image_and_anno_xmax3 nil)

(def ^:dynamic update_image_and_anno_xmin nil)

(def ^:dynamic update_image_and_anno_xmin1 nil)

(def ^:dynamic update_image_and_anno_xmin2 nil)

(def ^:dynamic update_image_and_anno_xmin3 nil)

(def ^:dynamic update_image_and_anno_y0 nil)

(def ^:dynamic update_image_and_anno_y1 nil)

(def ^:dynamic update_image_and_anno_y2 nil)

(def ^:dynamic update_image_and_anno_y3 nil)

(def ^:dynamic update_image_and_anno_ymax nil)

(def ^:dynamic update_image_and_anno_ymax1 nil)

(def ^:dynamic update_image_and_anno_ymax2 nil)

(def ^:dynamic update_image_and_anno_ymax3 nil)

(def ^:dynamic update_image_and_anno_ymin nil)

(def ^:dynamic update_image_and_anno_ymin1 nil)

(def ^:dynamic update_image_and_anno_ymin2 nil)

(def ^:dynamic update_image_and_anno_ymin3 nil)

(defn update_image_and_anno [update_image_and_anno_all_img_list update_image_and_anno_all_annos update_image_and_anno_idxs update_image_and_anno_output_size update_image_and_anno_scale_range update_image_and_anno_filter_scale]
  (binding [update_image_and_anno_anno nil update_image_and_anno_bbox nil update_image_and_anno_bbox1 nil update_image_and_anno_bbox2 nil update_image_and_anno_bbox3 nil update_image_and_anno_c nil update_image_and_anno_divid_point_x nil update_image_and_anno_divid_point_y nil update_image_and_anno_filtered nil update_image_and_anno_h nil update_image_and_anno_height nil update_image_and_anno_i nil update_image_and_anno_img_annos nil update_image_and_anno_index nil update_image_and_anno_j0 nil update_image_and_anno_j1 nil update_image_and_anno_j2 nil update_image_and_anno_j3 nil update_image_and_anno_k nil update_image_and_anno_new_anno nil update_image_and_anno_output_img nil update_image_and_anno_path nil update_image_and_anno_path_list nil update_image_and_anno_r nil update_image_and_anno_row nil update_image_and_anno_scale_x nil update_image_and_anno_scale_y nil update_image_and_anno_w nil update_image_and_anno_width nil update_image_and_anno_x0 nil update_image_and_anno_x1 nil update_image_and_anno_x2 nil update_image_and_anno_x3 nil update_image_and_anno_xmax nil update_image_and_anno_xmax1 nil update_image_and_anno_xmax2 nil update_image_and_anno_xmax3 nil update_image_and_anno_xmin nil update_image_and_anno_xmin1 nil update_image_and_anno_xmin2 nil update_image_and_anno_xmin3 nil update_image_and_anno_y0 nil update_image_and_anno_y1 nil update_image_and_anno_y2 nil update_image_and_anno_y3 nil update_image_and_anno_ymax nil update_image_and_anno_ymax1 nil update_image_and_anno_ymax2 nil update_image_and_anno_ymax3 nil update_image_and_anno_ymin nil update_image_and_anno_ymin1 nil update_image_and_anno_ymin2 nil update_image_and_anno_ymin3 nil] (try (do (set! update_image_and_anno_height (nth update_image_and_anno_output_size 0)) (set! update_image_and_anno_width (nth update_image_and_anno_output_size 1)) (set! update_image_and_anno_output_img nil) (set! update_image_and_anno_r 0) (while (< update_image_and_anno_r update_image_and_anno_height) (do (set! update_image_and_anno_row nil) (set! update_image_and_anno_c 0) (while (< update_image_and_anno_c update_image_and_anno_width) (do (set! update_image_and_anno_row (conj update_image_and_anno_row 0)) (set! update_image_and_anno_c (+ update_image_and_anno_c 1)))) (set! update_image_and_anno_output_img (conj update_image_and_anno_output_img update_image_and_anno_row)) (set! update_image_and_anno_r (+ update_image_and_anno_r 1)))) (set! update_image_and_anno_scale_x (/ (+ (nth update_image_and_anno_scale_range 0) (nth update_image_and_anno_scale_range 1)) 2.0)) (set! update_image_and_anno_scale_y (/ (+ (nth update_image_and_anno_scale_range 0) (nth update_image_and_anno_scale_range 1)) 2.0)) (set! update_image_and_anno_divid_point_x (long (* update_image_and_anno_scale_x (double update_image_and_anno_width)))) (set! update_image_and_anno_divid_point_y (long (* update_image_and_anno_scale_y (double update_image_and_anno_height)))) (set! update_image_and_anno_new_anno nil) (set! update_image_and_anno_path_list nil) (set! update_image_and_anno_i 0) (while (< update_image_and_anno_i (count update_image_and_anno_idxs)) (do (set! update_image_and_anno_index (nth update_image_and_anno_idxs update_image_and_anno_i)) (set! update_image_and_anno_path (nth update_image_and_anno_all_img_list update_image_and_anno_index)) (set! update_image_and_anno_path_list (conj update_image_and_anno_path_list update_image_and_anno_path)) (set! update_image_and_anno_img_annos (nth update_image_and_anno_all_annos update_image_and_anno_index)) (if (= update_image_and_anno_i 0) (do (set! update_image_and_anno_y0 0) (while (< update_image_and_anno_y0 update_image_and_anno_divid_point_y) (do (set! update_image_and_anno_x0 0) (while (< update_image_and_anno_x0 update_image_and_anno_divid_point_x) (do (set! update_image_and_anno_output_img (assoc-in update_image_and_anno_output_img [update_image_and_anno_y0 update_image_and_anno_x0] (+ update_image_and_anno_i 1))) (set! update_image_and_anno_x0 (+ update_image_and_anno_x0 1)))) (set! update_image_and_anno_y0 (+ update_image_and_anno_y0 1)))) (set! update_image_and_anno_j0 0) (while (< update_image_and_anno_j0 (count update_image_and_anno_img_annos)) (do (set! update_image_and_anno_bbox (nth update_image_and_anno_img_annos update_image_and_anno_j0)) (set! update_image_and_anno_xmin (* (nth update_image_and_anno_bbox 1) update_image_and_anno_scale_x)) (set! update_image_and_anno_ymin (* (nth update_image_and_anno_bbox 2) update_image_and_anno_scale_y)) (set! update_image_and_anno_xmax (* (nth update_image_and_anno_bbox 3) update_image_and_anno_scale_x)) (set! update_image_and_anno_ymax (* (nth update_image_and_anno_bbox 4) update_image_and_anno_scale_y)) (set! update_image_and_anno_new_anno (conj update_image_and_anno_new_anno [(nth update_image_and_anno_bbox 0) update_image_and_anno_xmin update_image_and_anno_ymin update_image_and_anno_xmax update_image_and_anno_ymax])) (set! update_image_and_anno_j0 (+ update_image_and_anno_j0 1))))) (if (= update_image_and_anno_i 1) (do (set! update_image_and_anno_y1 0) (while (< update_image_and_anno_y1 update_image_and_anno_divid_point_y) (do (set! update_image_and_anno_x1 update_image_and_anno_divid_point_x) (while (< update_image_and_anno_x1 update_image_and_anno_width) (do (set! update_image_and_anno_output_img (assoc-in update_image_and_anno_output_img [update_image_and_anno_y1 update_image_and_anno_x1] (+ update_image_and_anno_i 1))) (set! update_image_and_anno_x1 (+ update_image_and_anno_x1 1)))) (set! update_image_and_anno_y1 (+ update_image_and_anno_y1 1)))) (set! update_image_and_anno_j1 0) (while (< update_image_and_anno_j1 (count update_image_and_anno_img_annos)) (do (set! update_image_and_anno_bbox1 (nth update_image_and_anno_img_annos update_image_and_anno_j1)) (set! update_image_and_anno_xmin1 (+ update_image_and_anno_scale_x (* (nth update_image_and_anno_bbox1 1) (- 1.0 update_image_and_anno_scale_x)))) (set! update_image_and_anno_ymin1 (* (nth update_image_and_anno_bbox1 2) update_image_and_anno_scale_y)) (set! update_image_and_anno_xmax1 (+ update_image_and_anno_scale_x (* (nth update_image_and_anno_bbox1 3) (- 1.0 update_image_and_anno_scale_x)))) (set! update_image_and_anno_ymax1 (* (nth update_image_and_anno_bbox1 4) update_image_and_anno_scale_y)) (set! update_image_and_anno_new_anno (conj update_image_and_anno_new_anno [(nth update_image_and_anno_bbox1 0) update_image_and_anno_xmin1 update_image_and_anno_ymin1 update_image_and_anno_xmax1 update_image_and_anno_ymax1])) (set! update_image_and_anno_j1 (+ update_image_and_anno_j1 1))))) (if (= update_image_and_anno_i 2) (do (set! update_image_and_anno_y2 update_image_and_anno_divid_point_y) (while (< update_image_and_anno_y2 update_image_and_anno_height) (do (set! update_image_and_anno_x2 0) (while (< update_image_and_anno_x2 update_image_and_anno_divid_point_x) (do (set! update_image_and_anno_output_img (assoc-in update_image_and_anno_output_img [update_image_and_anno_y2 update_image_and_anno_x2] (+ update_image_and_anno_i 1))) (set! update_image_and_anno_x2 (+ update_image_and_anno_x2 1)))) (set! update_image_and_anno_y2 (+ update_image_and_anno_y2 1)))) (set! update_image_and_anno_j2 0) (while (< update_image_and_anno_j2 (count update_image_and_anno_img_annos)) (do (set! update_image_and_anno_bbox2 (nth update_image_and_anno_img_annos update_image_and_anno_j2)) (set! update_image_and_anno_xmin2 (* (nth update_image_and_anno_bbox2 1) update_image_and_anno_scale_x)) (set! update_image_and_anno_ymin2 (+ update_image_and_anno_scale_y (* (nth update_image_and_anno_bbox2 2) (- 1.0 update_image_and_anno_scale_y)))) (set! update_image_and_anno_xmax2 (* (nth update_image_and_anno_bbox2 3) update_image_and_anno_scale_x)) (set! update_image_and_anno_ymax2 (+ update_image_and_anno_scale_y (* (nth update_image_and_anno_bbox2 4) (- 1.0 update_image_and_anno_scale_y)))) (set! update_image_and_anno_new_anno (conj update_image_and_anno_new_anno [(nth update_image_and_anno_bbox2 0) update_image_and_anno_xmin2 update_image_and_anno_ymin2 update_image_and_anno_xmax2 update_image_and_anno_ymax2])) (set! update_image_and_anno_j2 (+ update_image_and_anno_j2 1))))) (do (set! update_image_and_anno_y3 update_image_and_anno_divid_point_y) (while (< update_image_and_anno_y3 update_image_and_anno_height) (do (set! update_image_and_anno_x3 update_image_and_anno_divid_point_x) (while (< update_image_and_anno_x3 update_image_and_anno_width) (do (set! update_image_and_anno_output_img (assoc-in update_image_and_anno_output_img [update_image_and_anno_y3 update_image_and_anno_x3] (+ update_image_and_anno_i 1))) (set! update_image_and_anno_x3 (+ update_image_and_anno_x3 1)))) (set! update_image_and_anno_y3 (+ update_image_and_anno_y3 1)))) (set! update_image_and_anno_j3 0) (while (< update_image_and_anno_j3 (count update_image_and_anno_img_annos)) (do (set! update_image_and_anno_bbox3 (nth update_image_and_anno_img_annos update_image_and_anno_j3)) (set! update_image_and_anno_xmin3 (+ update_image_and_anno_scale_x (* (nth update_image_and_anno_bbox3 1) (- 1.0 update_image_and_anno_scale_x)))) (set! update_image_and_anno_ymin3 (+ update_image_and_anno_scale_y (* (nth update_image_and_anno_bbox3 2) (- 1.0 update_image_and_anno_scale_y)))) (set! update_image_and_anno_xmax3 (+ update_image_and_anno_scale_x (* (nth update_image_and_anno_bbox3 3) (- 1.0 update_image_and_anno_scale_x)))) (set! update_image_and_anno_ymax3 (+ update_image_and_anno_scale_y (* (nth update_image_and_anno_bbox3 4) (- 1.0 update_image_and_anno_scale_y)))) (set! update_image_and_anno_new_anno (conj update_image_and_anno_new_anno [(nth update_image_and_anno_bbox3 0) update_image_and_anno_xmin3 update_image_and_anno_ymin3 update_image_and_anno_xmax3 update_image_and_anno_ymax3])) (set! update_image_and_anno_j3 (+ update_image_and_anno_j3 1)))))))) (set! update_image_and_anno_i (+ update_image_and_anno_i 1)))) (when (> update_image_and_anno_filter_scale 0.0) (do (set! update_image_and_anno_filtered nil) (set! update_image_and_anno_k 0) (while (< update_image_and_anno_k (count update_image_and_anno_new_anno)) (do (set! update_image_and_anno_anno (nth update_image_and_anno_new_anno update_image_and_anno_k)) (set! update_image_and_anno_w (- (nth update_image_and_anno_anno 3) (nth update_image_and_anno_anno 1))) (set! update_image_and_anno_h (- (nth update_image_and_anno_anno 4) (nth update_image_and_anno_anno 2))) (when (and (< update_image_and_anno_filter_scale update_image_and_anno_w) (< update_image_and_anno_filter_scale update_image_and_anno_h)) (set! update_image_and_anno_filtered (conj update_image_and_anno_filtered update_image_and_anno_anno))) (set! update_image_and_anno_k (+ update_image_and_anno_k 1)))) (set! update_image_and_anno_new_anno update_image_and_anno_filtered))) (throw (ex-info "return" {:v {:img update_image_and_anno_output_img :annos update_image_and_anno_new_anno :path (nth update_image_and_anno_path_list 0)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_a nil main_all_annos nil main_all_img_list nil main_filter_scale nil main_i nil main_idxs nil main_new_annos nil main_output_size nil main_path nil main_res nil main_scale_range nil] (do (set! main_all_img_list ["img0.jpg" "img1.jpg" "img2.jpg" "img3.jpg"]) (set! main_all_annos [[[0.0 0.1 0.1 0.4 0.4]] [[1.0 0.2 0.3 0.5 0.7]] [[2.0 0.6 0.2 0.9 0.5]] [[3.0 0.5 0.5 0.8 0.8]]]) (set! main_idxs [0 1 2 3]) (set! main_output_size [100 100]) (set! main_scale_range [0.4 0.6]) (set! main_filter_scale 0.05) (set! main_res (update_image_and_anno main_all_img_list main_all_annos main_idxs main_output_size main_scale_range main_filter_scale)) (set! main_new_annos (:annos main_res)) (set! main_path (:path main_res)) (println (str "Base image: " main_path)) (println (str "Mosaic annotation count: " (str (count main_new_annos)))) (set! main_i 0) (while (< main_i (count main_new_annos)) (do (set! main_a (nth main_new_annos main_i)) (println (str (str (str (str (str (str (str (str (str (nth main_a 0)) " ") (str (nth main_a 1))) " ") (str (nth main_a 2))) " ") (str (nth main_a 3))) " ") (str (nth main_a 4)))) (set! main_i (+ main_i 1)))))))

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
