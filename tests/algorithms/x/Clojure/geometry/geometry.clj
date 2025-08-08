(ns main (:refer-clojure :exclude [make_angle make_side ellipse_area ellipse_perimeter circle_area circle_perimeter circle_diameter circle_max_parts make_polygon polygon_add_side polygon_get_side polygon_set_side make_rectangle rectangle_perimeter rectangle_area make_square square_perimeter square_area main]))

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

(declare make_angle make_side ellipse_area ellipse_perimeter circle_area circle_perimeter circle_diameter circle_max_parts make_polygon polygon_add_side polygon_get_side polygon_set_side make_rectangle rectangle_perimeter rectangle_area make_square square_perimeter square_area main)

(def ^:dynamic circle_area_area nil)

(def ^:dynamic circle_area_e nil)

(def ^:dynamic circle_perimeter_e nil)

(def ^:dynamic circle_perimeter_per nil)

(def ^:dynamic main_a nil)

(def ^:dynamic main_c nil)

(def ^:dynamic main_e nil)

(def ^:dynamic main_q nil)

(def ^:dynamic main_r nil)

(def ^:dynamic main_s nil)

(def ^:dynamic make_polygon_s nil)

(def ^:dynamic make_rectangle_long nil)

(def ^:dynamic make_rectangle_p nil)

(def ^:dynamic make_rectangle_short nil)

(def ^:dynamic make_square_rect nil)

(def ^:dynamic polygon_add_side_p nil)

(def ^:dynamic polygon_set_side_p nil)

(def ^:dynamic polygon_set_side_tmp nil)

(def ^:dynamic square_area_a nil)

(def ^:dynamic square_perimeter_p nil)

(def ^:dynamic main_PI 3.141592653589793)

(defn make_angle [make_angle_deg]
  (try (do (when (or (< make_angle_deg 0.0) (> make_angle_deg 360.0)) (throw (Exception. "degrees must be between 0 and 360"))) (throw (ex-info "return" {:v {:degrees make_angle_deg}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn make_side [make_side_length make_side_angle]
  (try (do (when (<= make_side_length 0.0) (throw (Exception. "length must be positive"))) (throw (ex-info "return" {:v {:angle make_side_angle :length make_side_length :next (- 1)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn ellipse_area [ellipse_area_e]
  (try (throw (ex-info "return" {:v (* (* main_PI (:major ellipse_area_e)) (:minor ellipse_area_e))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn ellipse_perimeter [ellipse_perimeter_e]
  (try (throw (ex-info "return" {:v (* main_PI (+ (:major ellipse_perimeter_e) (:minor ellipse_perimeter_e)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn circle_area [circle_area_c]
  (binding [circle_area_area nil circle_area_e nil] (try (do (set! circle_area_e {:major (:radius circle_area_c) :minor (:radius circle_area_c)}) (set! circle_area_area (ellipse_area circle_area_e)) (throw (ex-info "return" {:v circle_area_area}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn circle_perimeter [circle_perimeter_c]
  (binding [circle_perimeter_e nil circle_perimeter_per nil] (try (do (set! circle_perimeter_e {:major (:radius circle_perimeter_c) :minor (:radius circle_perimeter_c)}) (set! circle_perimeter_per (ellipse_perimeter circle_perimeter_e)) (throw (ex-info "return" {:v circle_perimeter_per}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn circle_diameter [circle_diameter_c]
  (try (throw (ex-info "return" {:v (* (:radius circle_diameter_c) 2.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn circle_max_parts [circle_max_parts_num_cuts]
  (try (do (when (< circle_max_parts_num_cuts 0.0) (throw (Exception. "num_cuts must be positive"))) (throw (ex-info "return" {:v (* (+ (+ circle_max_parts_num_cuts 2.0) (* circle_max_parts_num_cuts circle_max_parts_num_cuts)) 0.5)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn make_polygon []
  (binding [make_polygon_s nil] (try (do (set! make_polygon_s []) (throw (ex-info "return" {:v {:sides make_polygon_s}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn polygon_add_side [polygon_add_side_p_p polygon_add_side_s]
  (binding [polygon_add_side_p nil] (do (set! polygon_add_side_p polygon_add_side_p_p) (set! polygon_add_side_p (assoc polygon_add_side_p :sides (conj (:sides polygon_add_side_p) polygon_add_side_s))))))

(defn polygon_get_side [polygon_get_side_p polygon_get_side_index]
  (try (throw (ex-info "return" {:v (get (:sides polygon_get_side_p) polygon_get_side_index)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn polygon_set_side [polygon_set_side_p_p polygon_set_side_index polygon_set_side_s]
  (binding [polygon_set_side_p nil polygon_set_side_tmp nil] (do (set! polygon_set_side_p polygon_set_side_p_p) (set! polygon_set_side_tmp (:sides polygon_set_side_p)) (set! polygon_set_side_tmp (assoc polygon_set_side_tmp polygon_set_side_index polygon_set_side_s)) (set! polygon_set_side_p (assoc polygon_set_side_p :sides polygon_set_side_tmp)))))

(defn make_rectangle [make_rectangle_short_len make_rectangle_long_len]
  (binding [make_rectangle_long nil make_rectangle_p nil make_rectangle_short nil] (try (do (when (or (<= make_rectangle_short_len 0.0) (<= make_rectangle_long_len 0.0)) (throw (Exception. "length must be positive"))) (set! make_rectangle_short (make_side make_rectangle_short_len (make_angle 90.0))) (set! make_rectangle_long (make_side make_rectangle_long_len (make_angle 90.0))) (set! make_rectangle_p (make_polygon)) (polygon_add_side make_rectangle_p make_rectangle_short) (polygon_add_side make_rectangle_p make_rectangle_long) (throw (ex-info "return" {:v {:long_side make_rectangle_long :poly make_rectangle_p :short_side make_rectangle_short}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rectangle_perimeter [rectangle_perimeter_r]
  (try (throw (ex-info "return" {:v (* (+ (:length (:short_side rectangle_perimeter_r)) (:length (:long_side rectangle_perimeter_r))) 2.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rectangle_area [rectangle_area_r]
  (try (throw (ex-info "return" {:v (* (:length (:short_side rectangle_area_r)) (:length (:long_side rectangle_area_r)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn make_square [make_square_side_len]
  (binding [make_square_rect nil] (try (do (set! make_square_rect (make_rectangle make_square_side_len make_square_side_len)) (throw (ex-info "return" {:v {:rect make_square_rect :side (:short_side make_square_rect)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn square_perimeter [square_perimeter_s]
  (binding [square_perimeter_p nil] (try (do (set! square_perimeter_p (rectangle_perimeter (:rect square_perimeter_s))) (throw (ex-info "return" {:v square_perimeter_p}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn square_area [square_area_s]
  (binding [square_area_a nil] (try (do (set! square_area_a (rectangle_area (:rect square_area_s))) (throw (ex-info "return" {:v square_area_a}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_a nil main_c nil main_e nil main_q nil main_r nil main_s nil] (do (set! main_a (make_angle 90.0)) (println (:degrees main_a)) (set! main_s (make_side 5.0 main_a)) (println (:length main_s)) (set! main_e {:major 5.0 :minor 10.0}) (println (ellipse_area main_e)) (println (ellipse_perimeter main_e)) (set! main_c {:radius 5.0}) (println (circle_area main_c)) (println (circle_perimeter main_c)) (println (circle_diameter main_c)) (println (circle_max_parts 7.0)) (set! main_r (make_rectangle 5.0 10.0)) (println (rectangle_perimeter main_r)) (println (rectangle_area main_r)) (set! main_q (make_square 5.0)) (println (square_perimeter main_q)) (println (square_area main_q)))))

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
