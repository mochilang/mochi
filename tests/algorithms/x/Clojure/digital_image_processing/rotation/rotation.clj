(ns main (:refer-clojure :exclude [mat_inverse3 mat_vec_mul create_matrix round_to_int get_rotation]))

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

(declare mat_inverse3 mat_vec_mul create_matrix round_to_int get_rotation)

(def ^:dynamic create_matrix_c nil)

(def ^:dynamic create_matrix_r nil)

(def ^:dynamic create_matrix_result nil)

(def ^:dynamic create_matrix_row nil)

(def ^:dynamic get_rotation_a0 nil)

(def ^:dynamic get_rotation_a1 nil)

(def ^:dynamic get_rotation_a2 nil)

(def ^:dynamic get_rotation_avec nil)

(def ^:dynamic get_rotation_b0 nil)

(def ^:dynamic get_rotation_b1 nil)

(def ^:dynamic get_rotation_b2 nil)

(def ^:dynamic get_rotation_bvec nil)

(def ^:dynamic get_rotation_inv nil)

(def ^:dynamic get_rotation_out nil)

(def ^:dynamic get_rotation_src nil)

(def ^:dynamic get_rotation_sx nil)

(def ^:dynamic get_rotation_sy nil)

(def ^:dynamic get_rotation_vecx nil)

(def ^:dynamic get_rotation_vecy nil)

(def ^:dynamic get_rotation_x nil)

(def ^:dynamic get_rotation_xf nil)

(def ^:dynamic get_rotation_y nil)

(def ^:dynamic get_rotation_yf nil)

(def ^:dynamic mat_inverse3_a nil)

(def ^:dynamic mat_inverse3_adj00 nil)

(def ^:dynamic mat_inverse3_adj01 nil)

(def ^:dynamic mat_inverse3_adj02 nil)

(def ^:dynamic mat_inverse3_adj10 nil)

(def ^:dynamic mat_inverse3_adj11 nil)

(def ^:dynamic mat_inverse3_adj12 nil)

(def ^:dynamic mat_inverse3_adj20 nil)

(def ^:dynamic mat_inverse3_adj21 nil)

(def ^:dynamic mat_inverse3_adj22 nil)

(def ^:dynamic mat_inverse3_b nil)

(def ^:dynamic mat_inverse3_c nil)

(def ^:dynamic mat_inverse3_d nil)

(def ^:dynamic mat_inverse3_det nil)

(def ^:dynamic mat_inverse3_e nil)

(def ^:dynamic mat_inverse3_f nil)

(def ^:dynamic mat_inverse3_g nil)

(def ^:dynamic mat_inverse3_h nil)

(def ^:dynamic mat_inverse3_i nil)

(def ^:dynamic mat_inverse3_inv nil)

(def ^:dynamic mat_vec_mul_i nil)

(def ^:dynamic mat_vec_mul_res nil)

(def ^:dynamic mat_vec_mul_val nil)

(defn mat_inverse3 [mat_inverse3_m]
  (binding [mat_inverse3_a nil mat_inverse3_adj00 nil mat_inverse3_adj01 nil mat_inverse3_adj02 nil mat_inverse3_adj10 nil mat_inverse3_adj11 nil mat_inverse3_adj12 nil mat_inverse3_adj20 nil mat_inverse3_adj21 nil mat_inverse3_adj22 nil mat_inverse3_b nil mat_inverse3_c nil mat_inverse3_d nil mat_inverse3_det nil mat_inverse3_e nil mat_inverse3_f nil mat_inverse3_g nil mat_inverse3_h nil mat_inverse3_i nil mat_inverse3_inv nil] (try (do (set! mat_inverse3_a (nth (nth mat_inverse3_m 0) 0)) (set! mat_inverse3_b (nth (nth mat_inverse3_m 0) 1)) (set! mat_inverse3_c (nth (nth mat_inverse3_m 0) 2)) (set! mat_inverse3_d (nth (nth mat_inverse3_m 1) 0)) (set! mat_inverse3_e (nth (nth mat_inverse3_m 1) 1)) (set! mat_inverse3_f (nth (nth mat_inverse3_m 1) 2)) (set! mat_inverse3_g (nth (nth mat_inverse3_m 2) 0)) (set! mat_inverse3_h (nth (nth mat_inverse3_m 2) 1)) (set! mat_inverse3_i (nth (nth mat_inverse3_m 2) 2)) (set! mat_inverse3_det (+ (- (* mat_inverse3_a (- (* mat_inverse3_e mat_inverse3_i) (* mat_inverse3_f mat_inverse3_h))) (* mat_inverse3_b (- (* mat_inverse3_d mat_inverse3_i) (* mat_inverse3_f mat_inverse3_g)))) (* mat_inverse3_c (- (* mat_inverse3_d mat_inverse3_h) (* mat_inverse3_e mat_inverse3_g))))) (when (= mat_inverse3_det 0.0) (throw (Exception. "singular matrix"))) (set! mat_inverse3_adj00 (- (* mat_inverse3_e mat_inverse3_i) (* mat_inverse3_f mat_inverse3_h))) (set! mat_inverse3_adj01 (- (* mat_inverse3_c mat_inverse3_h) (* mat_inverse3_b mat_inverse3_i))) (set! mat_inverse3_adj02 (- (* mat_inverse3_b mat_inverse3_f) (* mat_inverse3_c mat_inverse3_e))) (set! mat_inverse3_adj10 (- (* mat_inverse3_f mat_inverse3_g) (* mat_inverse3_d mat_inverse3_i))) (set! mat_inverse3_adj11 (- (* mat_inverse3_a mat_inverse3_i) (* mat_inverse3_c mat_inverse3_g))) (set! mat_inverse3_adj12 (- (* mat_inverse3_c mat_inverse3_d) (* mat_inverse3_a mat_inverse3_f))) (set! mat_inverse3_adj20 (- (* mat_inverse3_d mat_inverse3_h) (* mat_inverse3_e mat_inverse3_g))) (set! mat_inverse3_adj21 (- (* mat_inverse3_b mat_inverse3_g) (* mat_inverse3_a mat_inverse3_h))) (set! mat_inverse3_adj22 (- (* mat_inverse3_a mat_inverse3_e) (* mat_inverse3_b mat_inverse3_d))) (set! mat_inverse3_inv []) (set! mat_inverse3_inv (conj mat_inverse3_inv [(quot mat_inverse3_adj00 mat_inverse3_det) (quot mat_inverse3_adj01 mat_inverse3_det) (quot mat_inverse3_adj02 mat_inverse3_det)])) (set! mat_inverse3_inv (conj mat_inverse3_inv [(quot mat_inverse3_adj10 mat_inverse3_det) (quot mat_inverse3_adj11 mat_inverse3_det) (quot mat_inverse3_adj12 mat_inverse3_det)])) (set! mat_inverse3_inv (conj mat_inverse3_inv [(quot mat_inverse3_adj20 mat_inverse3_det) (quot mat_inverse3_adj21 mat_inverse3_det) (quot mat_inverse3_adj22 mat_inverse3_det)])) (throw (ex-info "return" {:v mat_inverse3_inv}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mat_vec_mul [mat_vec_mul_m mat_vec_mul_v]
  (binding [mat_vec_mul_i nil mat_vec_mul_res nil mat_vec_mul_val nil] (try (do (set! mat_vec_mul_res []) (set! mat_vec_mul_i 0) (while (< mat_vec_mul_i 3) (do (set! mat_vec_mul_val (+ (+ (* (nth (nth mat_vec_mul_m mat_vec_mul_i) 0) (nth mat_vec_mul_v 0)) (* (nth (nth mat_vec_mul_m mat_vec_mul_i) 1) (nth mat_vec_mul_v 1))) (* (nth (nth mat_vec_mul_m mat_vec_mul_i) 2) (nth mat_vec_mul_v 2)))) (set! mat_vec_mul_res (conj mat_vec_mul_res mat_vec_mul_val)) (set! mat_vec_mul_i (+ mat_vec_mul_i 1)))) (throw (ex-info "return" {:v mat_vec_mul_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn create_matrix [create_matrix_rows create_matrix_cols create_matrix_value]
  (binding [create_matrix_c nil create_matrix_r nil create_matrix_result nil create_matrix_row nil] (try (do (set! create_matrix_result []) (set! create_matrix_r 0) (while (< create_matrix_r create_matrix_rows) (do (set! create_matrix_row []) (set! create_matrix_c 0) (while (< create_matrix_c create_matrix_cols) (do (set! create_matrix_row (conj create_matrix_row create_matrix_value)) (set! create_matrix_c (+ create_matrix_c 1)))) (set! create_matrix_result (conj create_matrix_result create_matrix_row)) (set! create_matrix_r (+ create_matrix_r 1)))) (throw (ex-info "return" {:v create_matrix_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn round_to_int [round_to_int_x]
  (try (if (>= round_to_int_x 0.0) (Integer/parseInt (+ round_to_int_x 0.5)) (Integer/parseInt (- round_to_int_x 0.5))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_rotation [get_rotation_img get_rotation_pt1 get_rotation_pt2 get_rotation_rows get_rotation_cols]
  (binding [get_rotation_a0 nil get_rotation_a1 nil get_rotation_a2 nil get_rotation_avec nil get_rotation_b0 nil get_rotation_b1 nil get_rotation_b2 nil get_rotation_bvec nil get_rotation_inv nil get_rotation_out nil get_rotation_src nil get_rotation_sx nil get_rotation_sy nil get_rotation_vecx nil get_rotation_vecy nil get_rotation_x nil get_rotation_xf nil get_rotation_y nil get_rotation_yf nil] (try (do (set! get_rotation_src [[(nth (nth get_rotation_pt1 0) 0) (nth (nth get_rotation_pt1 0) 1) 1.0] [(nth (nth get_rotation_pt1 1) 0) (nth (nth get_rotation_pt1 1) 1) 1.0] [(nth (nth get_rotation_pt1 2) 0) (nth (nth get_rotation_pt1 2) 1) 1.0]]) (set! get_rotation_inv (mat_inverse3 get_rotation_src)) (set! get_rotation_vecx [(nth (nth get_rotation_pt2 0) 0) (nth (nth get_rotation_pt2 1) 0) (nth (nth get_rotation_pt2 2) 0)]) (set! get_rotation_vecy [(nth (nth get_rotation_pt2 0) 1) (nth (nth get_rotation_pt2 1) 1) (nth (nth get_rotation_pt2 2) 1)]) (set! get_rotation_avec (mat_vec_mul get_rotation_inv get_rotation_vecx)) (set! get_rotation_bvec (mat_vec_mul get_rotation_inv get_rotation_vecy)) (set! get_rotation_a0 (nth get_rotation_avec 0)) (set! get_rotation_a1 (nth get_rotation_avec 1)) (set! get_rotation_a2 (nth get_rotation_avec 2)) (set! get_rotation_b0 (nth get_rotation_bvec 0)) (set! get_rotation_b1 (nth get_rotation_bvec 1)) (set! get_rotation_b2 (nth get_rotation_bvec 2)) (set! get_rotation_out (create_matrix get_rotation_rows get_rotation_cols 0)) (set! get_rotation_y 0) (while (< get_rotation_y get_rotation_rows) (do (set! get_rotation_x 0) (while (< get_rotation_x get_rotation_cols) (do (set! get_rotation_xf (+ (+ (* get_rotation_a0 (* 1.0 get_rotation_x)) (* get_rotation_a1 (* 1.0 get_rotation_y))) get_rotation_a2)) (set! get_rotation_yf (+ (+ (* get_rotation_b0 (* 1.0 get_rotation_x)) (* get_rotation_b1 (* 1.0 get_rotation_y))) get_rotation_b2)) (set! get_rotation_sx (round_to_int get_rotation_xf)) (set! get_rotation_sy (round_to_int get_rotation_yf)) (when (and (and (and (>= get_rotation_sx 0) (< get_rotation_sx get_rotation_cols)) (>= get_rotation_sy 0)) (< get_rotation_sy get_rotation_rows)) (set! get_rotation_out (assoc-in get_rotation_out [get_rotation_sy get_rotation_sx] (nth (nth get_rotation_img get_rotation_y) get_rotation_x)))) (set! get_rotation_x (+ get_rotation_x 1)))) (set! get_rotation_y (+ get_rotation_y 1)))) (throw (ex-info "return" {:v get_rotation_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_img [[1 2 3] [4 5 6] [7 8 9]])

(def ^:dynamic main_pts1 [[0.0 0.0] [2.0 0.0] [0.0 2.0]])

(def ^:dynamic main_pts2 [[0.0 2.0] [0.0 0.0] [2.0 2.0]])

(def ^:dynamic main_rotated (get_rotation main_img main_pts1 main_pts2 3 3))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str main_rotated))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
