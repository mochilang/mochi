(ns main (:refer-clojure :exclude [_mod sin_approx cos_approx tan_approx sqrt_approx surface_area_cube surface_area_cuboid surface_area_sphere surface_area_hemisphere surface_area_cone surface_area_conical_frustum surface_area_cylinder surface_area_torus area_rectangle area_square area_triangle area_triangle_three_sides area_parallelogram area_trapezium area_circle area_ellipse area_rhombus area_reg_polygon]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (Integer/parseInt (str s)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare _mod sin_approx cos_approx tan_approx sqrt_approx surface_area_cube surface_area_cuboid surface_area_sphere surface_area_hemisphere surface_area_cone surface_area_conical_frustum surface_area_cylinder surface_area_torus area_rectangle area_square area_triangle area_triangle_three_sides area_parallelogram area_trapezium area_circle area_ellipse area_rhombus area_reg_polygon)

(def ^:dynamic area_reg_polygon_n nil)

(def ^:dynamic area_triangle_three_sides_prod nil)

(def ^:dynamic area_triangle_three_sides_res nil)

(def ^:dynamic area_triangle_three_sides_s nil)

(def ^:dynamic cos_approx_y nil)

(def ^:dynamic cos_approx_y2 nil)

(def ^:dynamic cos_approx_y4 nil)

(def ^:dynamic cos_approx_y6 nil)

(def ^:dynamic sin_approx_y nil)

(def ^:dynamic sin_approx_y2 nil)

(def ^:dynamic sin_approx_y3 nil)

(def ^:dynamic sin_approx_y5 nil)

(def ^:dynamic sin_approx_y7 nil)

(def ^:dynamic sqrt_approx_guess nil)

(def ^:dynamic sqrt_approx_i nil)

(def ^:dynamic surface_area_cone_slant nil)

(def ^:dynamic surface_area_conical_frustum_slant nil)

(def ^:dynamic main_PI 3.141592653589793)

(def ^:dynamic main_TWO_PI 6.283185307179586)

(defn _mod [_mod_x _mod_m]
  (try (throw (ex-info "return" {:v (- _mod_x (* (double (int (quot _mod_x _mod_m))) _mod_m))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sin_approx [sin_approx_x]
  (binding [sin_approx_y nil sin_approx_y2 nil sin_approx_y3 nil sin_approx_y5 nil sin_approx_y7 nil] (try (do (set! sin_approx_y (- (_mod (+ sin_approx_x main_PI) main_TWO_PI) main_PI)) (set! sin_approx_y2 (* sin_approx_y sin_approx_y)) (set! sin_approx_y3 (* sin_approx_y2 sin_approx_y)) (set! sin_approx_y5 (* sin_approx_y3 sin_approx_y2)) (set! sin_approx_y7 (* sin_approx_y5 sin_approx_y2)) (throw (ex-info "return" {:v (- (+ (- sin_approx_y (/ sin_approx_y3 6.0)) (/ sin_approx_y5 120.0)) (/ sin_approx_y7 5040.0))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cos_approx [cos_approx_x]
  (binding [cos_approx_y nil cos_approx_y2 nil cos_approx_y4 nil cos_approx_y6 nil] (try (do (set! cos_approx_y (- (_mod (+ cos_approx_x main_PI) main_TWO_PI) main_PI)) (set! cos_approx_y2 (* cos_approx_y cos_approx_y)) (set! cos_approx_y4 (* cos_approx_y2 cos_approx_y2)) (set! cos_approx_y6 (* cos_approx_y4 cos_approx_y2)) (throw (ex-info "return" {:v (- (+ (- 1.0 (/ cos_approx_y2 2.0)) (/ cos_approx_y4 24.0)) (/ cos_approx_y6 720.0))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn tan_approx [tan_approx_x]
  (try (throw (ex-info "return" {:v (/ (sin_approx tan_approx_x) (cos_approx tan_approx_x))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrt_approx [sqrt_approx_x]
  (binding [sqrt_approx_guess nil sqrt_approx_i nil] (try (do (when (<= sqrt_approx_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrt_approx_guess (/ sqrt_approx_x 2.0)) (set! sqrt_approx_i 0) (while (< sqrt_approx_i 20) (do (set! sqrt_approx_guess (/ (+ sqrt_approx_guess (quot sqrt_approx_x sqrt_approx_guess)) 2.0)) (set! sqrt_approx_i (+ sqrt_approx_i 1)))) (throw (ex-info "return" {:v sqrt_approx_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn surface_area_cube [surface_area_cube_side_length]
  (try (do (when (< surface_area_cube_side_length 0.0) (do (println "ValueError: surface_area_cube() only accepts non-negative values") (throw (ex-info "return" {:v 0.0})))) (throw (ex-info "return" {:v (* (* 6.0 surface_area_cube_side_length) surface_area_cube_side_length)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn surface_area_cuboid [surface_area_cuboid_length surface_area_cuboid_breadth surface_area_cuboid_height]
  (try (do (when (or (or (< surface_area_cuboid_length 0.0) (< surface_area_cuboid_breadth 0.0)) (< surface_area_cuboid_height 0.0)) (do (println "ValueError: surface_area_cuboid() only accepts non-negative values") (throw (ex-info "return" {:v 0.0})))) (throw (ex-info "return" {:v (* 2.0 (+ (+ (* surface_area_cuboid_length surface_area_cuboid_breadth) (* surface_area_cuboid_breadth surface_area_cuboid_height)) (* surface_area_cuboid_length surface_area_cuboid_height)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn surface_area_sphere [surface_area_sphere_radius]
  (try (do (when (< surface_area_sphere_radius 0.0) (do (println "ValueError: surface_area_sphere() only accepts non-negative values") (throw (ex-info "return" {:v 0.0})))) (throw (ex-info "return" {:v (* (* (* 4.0 main_PI) surface_area_sphere_radius) surface_area_sphere_radius)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn surface_area_hemisphere [surface_area_hemisphere_radius]
  (try (do (when (< surface_area_hemisphere_radius 0.0) (do (println "ValueError: surface_area_hemisphere() only accepts non-negative values") (throw (ex-info "return" {:v 0.0})))) (throw (ex-info "return" {:v (* (* (* 3.0 main_PI) surface_area_hemisphere_radius) surface_area_hemisphere_radius)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn surface_area_cone [surface_area_cone_radius surface_area_cone_height]
  (binding [surface_area_cone_slant nil] (try (do (when (or (< surface_area_cone_radius 0.0) (< surface_area_cone_height 0.0)) (do (println "ValueError: surface_area_cone() only accepts non-negative values") (throw (ex-info "return" {:v 0.0})))) (set! surface_area_cone_slant (sqrt_approx (+ (* surface_area_cone_height surface_area_cone_height) (* surface_area_cone_radius surface_area_cone_radius)))) (throw (ex-info "return" {:v (* (* main_PI surface_area_cone_radius) (+ surface_area_cone_radius surface_area_cone_slant))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn surface_area_conical_frustum [surface_area_conical_frustum_radius1 surface_area_conical_frustum_radius2 surface_area_conical_frustum_height]
  (binding [surface_area_conical_frustum_slant nil] (try (do (when (or (or (< surface_area_conical_frustum_radius1 0.0) (< surface_area_conical_frustum_radius2 0.0)) (< surface_area_conical_frustum_height 0.0)) (do (println "ValueError: surface_area_conical_frustum() only accepts non-negative values") (throw (ex-info "return" {:v 0.0})))) (set! surface_area_conical_frustum_slant (sqrt_approx (+ (* surface_area_conical_frustum_height surface_area_conical_frustum_height) (* (- surface_area_conical_frustum_radius1 surface_area_conical_frustum_radius2) (- surface_area_conical_frustum_radius1 surface_area_conical_frustum_radius2))))) (throw (ex-info "return" {:v (* main_PI (+ (+ (* surface_area_conical_frustum_slant (+ surface_area_conical_frustum_radius1 surface_area_conical_frustum_radius2)) (* surface_area_conical_frustum_radius1 surface_area_conical_frustum_radius1)) (* surface_area_conical_frustum_radius2 surface_area_conical_frustum_radius2)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn surface_area_cylinder [surface_area_cylinder_radius surface_area_cylinder_height]
  (try (do (when (or (< surface_area_cylinder_radius 0.0) (< surface_area_cylinder_height 0.0)) (do (println "ValueError: surface_area_cylinder() only accepts non-negative values") (throw (ex-info "return" {:v 0.0})))) (throw (ex-info "return" {:v (* (* (* 2.0 main_PI) surface_area_cylinder_radius) (+ surface_area_cylinder_height surface_area_cylinder_radius))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn surface_area_torus [surface_area_torus_torus_radius surface_area_torus_tube_radius]
  (try (do (when (or (< surface_area_torus_torus_radius 0.0) (< surface_area_torus_tube_radius 0.0)) (do (println "ValueError: surface_area_torus() only accepts non-negative values") (throw (ex-info "return" {:v 0.0})))) (when (< surface_area_torus_torus_radius surface_area_torus_tube_radius) (do (println "ValueError: surface_area_torus() does not support spindle or self intersecting tori") (throw (ex-info "return" {:v 0.0})))) (throw (ex-info "return" {:v (* (* (* (* 4.0 main_PI) main_PI) surface_area_torus_torus_radius) surface_area_torus_tube_radius)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn area_rectangle [area_rectangle_length area_rectangle_width]
  (try (do (when (or (< area_rectangle_length 0.0) (< area_rectangle_width 0.0)) (do (println "ValueError: area_rectangle() only accepts non-negative values") (throw (ex-info "return" {:v 0.0})))) (throw (ex-info "return" {:v (* area_rectangle_length area_rectangle_width)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn area_square [area_square_side_length]
  (try (do (when (< area_square_side_length 0.0) (do (println "ValueError: area_square() only accepts non-negative values") (throw (ex-info "return" {:v 0.0})))) (throw (ex-info "return" {:v (* area_square_side_length area_square_side_length)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn area_triangle [area_triangle_base area_triangle_height]
  (try (do (when (or (< area_triangle_base 0.0) (< area_triangle_height 0.0)) (do (println "ValueError: area_triangle() only accepts non-negative values") (throw (ex-info "return" {:v 0.0})))) (throw (ex-info "return" {:v (/ (* area_triangle_base area_triangle_height) 2.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn area_triangle_three_sides [area_triangle_three_sides_side1 area_triangle_three_sides_side2 area_triangle_three_sides_side3]
  (binding [area_triangle_three_sides_prod nil area_triangle_three_sides_res nil area_triangle_three_sides_s nil] (try (do (when (or (or (< area_triangle_three_sides_side1 0.0) (< area_triangle_three_sides_side2 0.0)) (< area_triangle_three_sides_side3 0.0)) (do (println "ValueError: area_triangle_three_sides() only accepts non-negative values") (throw (ex-info "return" {:v 0.0})))) (when (or (or (< (+ area_triangle_three_sides_side1 area_triangle_three_sides_side2) area_triangle_three_sides_side3) (< (+ area_triangle_three_sides_side1 area_triangle_three_sides_side3) area_triangle_three_sides_side2)) (< (+ area_triangle_three_sides_side2 area_triangle_three_sides_side3) area_triangle_three_sides_side1)) (do (println "ValueError: Given three sides do not form a triangle") (throw (ex-info "return" {:v 0.0})))) (set! area_triangle_three_sides_s (/ (+ (+ area_triangle_three_sides_side1 area_triangle_three_sides_side2) area_triangle_three_sides_side3) 2.0)) (set! area_triangle_three_sides_prod (* (* (* area_triangle_three_sides_s (- area_triangle_three_sides_s area_triangle_three_sides_side1)) (- area_triangle_three_sides_s area_triangle_three_sides_side2)) (- area_triangle_three_sides_s area_triangle_three_sides_side3))) (set! area_triangle_three_sides_res (sqrt_approx area_triangle_three_sides_prod)) (throw (ex-info "return" {:v area_triangle_three_sides_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn area_parallelogram [area_parallelogram_base area_parallelogram_height]
  (try (do (when (or (< area_parallelogram_base 0.0) (< area_parallelogram_height 0.0)) (do (println "ValueError: area_parallelogram() only accepts non-negative values") (throw (ex-info "return" {:v 0.0})))) (throw (ex-info "return" {:v (* area_parallelogram_base area_parallelogram_height)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn area_trapezium [area_trapezium_base1 area_trapezium_base2 area_trapezium_height]
  (try (do (when (or (or (< area_trapezium_base1 0.0) (< area_trapezium_base2 0.0)) (< area_trapezium_height 0.0)) (do (println "ValueError: area_trapezium() only accepts non-negative values") (throw (ex-info "return" {:v 0.0})))) (throw (ex-info "return" {:v (* (* 0.5 (+ area_trapezium_base1 area_trapezium_base2)) area_trapezium_height)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn area_circle [area_circle_radius]
  (try (do (when (< area_circle_radius 0.0) (do (println "ValueError: area_circle() only accepts non-negative values") (throw (ex-info "return" {:v 0.0})))) (throw (ex-info "return" {:v (* (* main_PI area_circle_radius) area_circle_radius)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn area_ellipse [area_ellipse_radius_x area_ellipse_radius_y]
  (try (do (when (or (< area_ellipse_radius_x 0.0) (< area_ellipse_radius_y 0.0)) (do (println "ValueError: area_ellipse() only accepts non-negative values") (throw (ex-info "return" {:v 0.0})))) (throw (ex-info "return" {:v (* (* main_PI area_ellipse_radius_x) area_ellipse_radius_y)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn area_rhombus [area_rhombus_diagonal1 area_rhombus_diagonal2]
  (try (do (when (or (< area_rhombus_diagonal1 0.0) (< area_rhombus_diagonal2 0.0)) (do (println "ValueError: area_rhombus() only accepts non-negative values") (throw (ex-info "return" {:v 0.0})))) (throw (ex-info "return" {:v (* (* 0.5 area_rhombus_diagonal1) area_rhombus_diagonal2)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn area_reg_polygon [area_reg_polygon_sides area_reg_polygon_length]
  (binding [area_reg_polygon_n nil] (try (do (when (< area_reg_polygon_sides 3) (do (println "ValueError: area_reg_polygon() only accepts integers greater than or equal to three as number of sides") (throw (ex-info "return" {:v 0.0})))) (when (< area_reg_polygon_length 0.0) (do (println "ValueError: area_reg_polygon() only accepts non-negative values as length of a side") (throw (ex-info "return" {:v 0.0})))) (set! area_reg_polygon_n (double area_reg_polygon_sides)) (throw (ex-info "return" {:v (/ (* (* area_reg_polygon_n area_reg_polygon_length) area_reg_polygon_length) (* 4.0 (tan_approx (/ main_PI area_reg_polygon_n))))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_TRI_THREE_SIDES (area_triangle_three_sides 5.0 12.0 13.0))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "[DEMO] Areas of various geometric shapes:")
      (println (str "Rectangle: " (str (area_rectangle 10.0 20.0))))
      (println (str "Square: " (str (area_square 10.0))))
      (println (str "Triangle: " (str (area_triangle 10.0 10.0))))
      (println (str "Triangle Three Sides: " (str main_TRI_THREE_SIDES)))
      (println (str "Parallelogram: " (str (area_parallelogram 10.0 20.0))))
      (println (str "Rhombus: " (str (area_rhombus 10.0 20.0))))
      (println (str "Trapezium: " (str (area_trapezium 10.0 20.0 30.0))))
      (println (str "Circle: " (str (area_circle 20.0))))
      (println (str "Ellipse: " (str (area_ellipse 10.0 20.0))))
      (println "")
      (println "Surface Areas of various geometric shapes:")
      (println (str "Cube: " (str (surface_area_cube 20.0))))
      (println (str "Cuboid: " (str (surface_area_cuboid 10.0 20.0 30.0))))
      (println (str "Sphere: " (str (surface_area_sphere 20.0))))
      (println (str "Hemisphere: " (str (surface_area_hemisphere 20.0))))
      (println (str "Cone: " (str (surface_area_cone 10.0 20.0))))
      (println (str "Conical Frustum: " (str (surface_area_conical_frustum 10.0 20.0 30.0))))
      (println (str "Cylinder: " (str (surface_area_cylinder 10.0 20.0))))
      (println (str "Torus: " (str (surface_area_torus 20.0 10.0))))
      (println (str "Equilateral Triangle: " (str (area_reg_polygon 3 10.0))))
      (println (str "Square: " (str (area_reg_polygon 4 10.0))))
      (println (str "Regular Pentagon: " (str (area_reg_polygon 5 10.0))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
