(ns main (:refer-clojure :exclude [round_int hsv_to_rgb get_distance get_black_and_white_rgb get_color_coded_rgb get_image rgb_to_string]))

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

(declare round_int hsv_to_rgb get_distance get_black_and_white_rgb get_color_coded_rgb get_image rgb_to_string)

(def ^:dynamic get_distance_a nil)

(def ^:dynamic get_distance_a_new nil)

(def ^:dynamic get_distance_b nil)

(def ^:dynamic get_distance_step nil)

(def ^:dynamic get_image_distance nil)

(def ^:dynamic get_image_figure_height nil)

(def ^:dynamic get_image_fx nil)

(def ^:dynamic get_image_fy nil)

(def ^:dynamic get_image_image_x nil)

(def ^:dynamic get_image_image_y nil)

(def ^:dynamic get_image_img nil)

(def ^:dynamic get_image_rgb nil)

(def ^:dynamic get_image_row nil)

(def ^:dynamic hsv_to_rgb_b nil)

(def ^:dynamic hsv_to_rgb_f nil)

(def ^:dynamic hsv_to_rgb_g nil)

(def ^:dynamic hsv_to_rgb_i nil)

(def ^:dynamic hsv_to_rgb_mod nil)

(def ^:dynamic hsv_to_rgb_p nil)

(def ^:dynamic hsv_to_rgb_q nil)

(def ^:dynamic hsv_to_rgb_r nil)

(def ^:dynamic hsv_to_rgb_t nil)

(defn round_int [round_int_x]
  (try (throw (ex-info "return" {:v (long (+ round_int_x 0.5))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn hsv_to_rgb [hsv_to_rgb_h hsv_to_rgb_s hsv_to_rgb_v]
  (binding [hsv_to_rgb_b nil hsv_to_rgb_f nil hsv_to_rgb_g nil hsv_to_rgb_i nil hsv_to_rgb_mod nil hsv_to_rgb_p nil hsv_to_rgb_q nil hsv_to_rgb_r nil hsv_to_rgb_t nil] (try (do (set! hsv_to_rgb_i (long (* hsv_to_rgb_h 6.0))) (set! hsv_to_rgb_f (- (* hsv_to_rgb_h 6.0) (double hsv_to_rgb_i))) (set! hsv_to_rgb_p (* hsv_to_rgb_v (- 1.0 hsv_to_rgb_s))) (set! hsv_to_rgb_q (* hsv_to_rgb_v (- 1.0 (* hsv_to_rgb_f hsv_to_rgb_s)))) (set! hsv_to_rgb_t (* hsv_to_rgb_v (- 1.0 (* (- 1.0 hsv_to_rgb_f) hsv_to_rgb_s)))) (set! hsv_to_rgb_mod (mod hsv_to_rgb_i 6)) (set! hsv_to_rgb_r 0.0) (set! hsv_to_rgb_g 0.0) (set! hsv_to_rgb_b 0.0) (if (= hsv_to_rgb_mod 0) (do (set! hsv_to_rgb_r hsv_to_rgb_v) (set! hsv_to_rgb_g hsv_to_rgb_t) (set! hsv_to_rgb_b hsv_to_rgb_p)) (if (= hsv_to_rgb_mod 1) (do (set! hsv_to_rgb_r hsv_to_rgb_q) (set! hsv_to_rgb_g hsv_to_rgb_v) (set! hsv_to_rgb_b hsv_to_rgb_p)) (if (= hsv_to_rgb_mod 2) (do (set! hsv_to_rgb_r hsv_to_rgb_p) (set! hsv_to_rgb_g hsv_to_rgb_v) (set! hsv_to_rgb_b hsv_to_rgb_t)) (if (= hsv_to_rgb_mod 3) (do (set! hsv_to_rgb_r hsv_to_rgb_p) (set! hsv_to_rgb_g hsv_to_rgb_q) (set! hsv_to_rgb_b hsv_to_rgb_v)) (if (= hsv_to_rgb_mod 4) (do (set! hsv_to_rgb_r hsv_to_rgb_t) (set! hsv_to_rgb_g hsv_to_rgb_p) (set! hsv_to_rgb_b hsv_to_rgb_v)) (do (set! hsv_to_rgb_r hsv_to_rgb_v) (set! hsv_to_rgb_g hsv_to_rgb_p) (set! hsv_to_rgb_b hsv_to_rgb_q))))))) (throw (ex-info "return" {:v {:b (round_int (* hsv_to_rgb_b 255.0)) :g (round_int (* hsv_to_rgb_g 255.0)) :r (round_int (* hsv_to_rgb_r 255.0))}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_distance [get_distance_x get_distance_y get_distance_max_step]
  (binding [get_distance_a nil get_distance_a_new nil get_distance_b nil get_distance_step nil] (try (do (set! get_distance_a get_distance_x) (set! get_distance_b get_distance_y) (set! get_distance_step (- 1)) (loop [while_flag_1 true] (when (and while_flag_1 (< get_distance_step (- get_distance_max_step 1))) (do (set! get_distance_step (+ get_distance_step 1)) (set! get_distance_a_new (+ (- (* get_distance_a get_distance_a) (* get_distance_b get_distance_b)) get_distance_x)) (set! get_distance_b (+ (* (* 2.0 get_distance_a) get_distance_b) get_distance_y)) (set! get_distance_a get_distance_a_new) (cond (> (+ (* get_distance_a get_distance_a) (* get_distance_b get_distance_b)) 4.0) (recur false) :else (recur while_flag_1))))) (throw (ex-info "return" {:v (quot (double get_distance_step) (double (- get_distance_max_step 1)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_black_and_white_rgb [get_black_and_white_rgb_distance]
  (try (if (= get_black_and_white_rgb_distance 1.0) (throw (ex-info "return" {:v {:b 0 :g 0 :r 0}})) (throw (ex-info "return" {:v {:b 255 :g 255 :r 255}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_color_coded_rgb [get_color_coded_rgb_distance]
  (try (if (= get_color_coded_rgb_distance 1.0) (throw (ex-info "return" {:v {:b 0 :g 0 :r 0}})) (throw (ex-info "return" {:v (hsv_to_rgb get_color_coded_rgb_distance 1.0 1.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_image [get_image_image_width get_image_image_height get_image_figure_center_x get_image_figure_center_y get_image_figure_width get_image_max_step get_image_use_distance_color_coding]
  (binding [get_image_distance nil get_image_figure_height nil get_image_fx nil get_image_fy nil get_image_image_x nil get_image_image_y nil get_image_img nil get_image_rgb nil get_image_row nil] (try (do (set! get_image_img []) (set! get_image_figure_height (* (quot get_image_figure_width (double get_image_image_width)) (double get_image_image_height))) (set! get_image_image_y 0) (while (< get_image_image_y get_image_image_height) (do (set! get_image_row []) (set! get_image_image_x 0) (while (< get_image_image_x get_image_image_width) (do (set! get_image_fx (+ get_image_figure_center_x (* (- (quot (double get_image_image_x) (double get_image_image_width)) 0.5) get_image_figure_width))) (set! get_image_fy (+ get_image_figure_center_y (* (- (quot (double get_image_image_y) (double get_image_image_height)) 0.5) get_image_figure_height))) (set! get_image_distance (get_distance get_image_fx get_image_fy get_image_max_step)) (set! get_image_rgb nil) (if get_image_use_distance_color_coding (set! get_image_rgb (get_color_coded_rgb get_image_distance)) (set! get_image_rgb (get_black_and_white_rgb get_image_distance))) (set! get_image_row (conj get_image_row get_image_rgb)) (set! get_image_image_x (+ get_image_image_x 1)))) (set! get_image_img (conj get_image_img get_image_row)) (set! get_image_image_y (+ get_image_image_y 1)))) (throw (ex-info "return" {:v get_image_img}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rgb_to_string [rgb_to_string_c]
  (try (throw (ex-info "return" {:v (str (str (str (str (str (str "(" (str (:r rgb_to_string_c))) ", ") (str (:g rgb_to_string_c))) ", ") (str (:b rgb_to_string_c))) ")")})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_img1 (get_image 10 10 (- 0.6) 0.0 3.2 50 true))

(def ^:dynamic main_img2 (get_image 10 10 (- 0.6) 0.0 3.2 50 false))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (rgb_to_string (nth (nth main_img1 0) 0)))
      (println (rgb_to_string (nth (nth main_img2 0) 0)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
