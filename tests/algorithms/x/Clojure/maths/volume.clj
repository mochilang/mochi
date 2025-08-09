(ns main (:refer-clojure :exclude [minf maxf vol_cube vol_spherical_cap vol_sphere vol_spheres_intersect vol_spheres_union vol_cuboid vol_cone vol_right_circ_cone vol_prism vol_pyramid vol_hemisphere vol_circular_cylinder vol_hollow_circular_cylinder vol_conical_frustum vol_torus vol_icosahedron main]))

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

(declare minf maxf vol_cube vol_spherical_cap vol_sphere vol_spheres_intersect vol_spheres_union vol_cuboid vol_cone vol_right_circ_cone vol_prism vol_pyramid vol_hemisphere vol_circular_cylinder vol_hollow_circular_cylinder vol_conical_frustum vol_torus vol_icosahedron main)

(def ^:dynamic vol_spheres_intersect_h1 nil)

(def ^:dynamic vol_spheres_intersect_h2 nil)

(def ^:dynamic main_PI 3.141592653589793)

(def ^:dynamic main_SQRT5 2.23606797749979)

(defn minf [minf_a minf_b]
  (try (if (< minf_a minf_b) minf_a minf_b) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn maxf [maxf_a maxf_b]
  (try (if (> maxf_a maxf_b) maxf_a maxf_b) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn vol_cube [vol_cube_side_length]
  (try (do (when (< vol_cube_side_length 0.0) (throw (Exception. "vol_cube() only accepts non-negative values"))) (throw (ex-info "return" {:v (* (* vol_cube_side_length vol_cube_side_length) vol_cube_side_length)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn vol_spherical_cap [vol_spherical_cap_height vol_spherical_cap_radius]
  (try (do (when (or (< vol_spherical_cap_height 0.0) (< vol_spherical_cap_radius 0.0)) (throw (Exception. "vol_spherical_cap() only accepts non-negative values"))) (throw (ex-info "return" {:v (* (* (* (* (/ 1.0 3.0) main_PI) vol_spherical_cap_height) vol_spherical_cap_height) (- (* 3.0 vol_spherical_cap_radius) vol_spherical_cap_height))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn vol_sphere [vol_sphere_radius]
  (try (do (when (< vol_sphere_radius 0.0) (throw (Exception. "vol_sphere() only accepts non-negative values"))) (throw (ex-info "return" {:v (* (* (* (* (/ 4.0 3.0) main_PI) vol_sphere_radius) vol_sphere_radius) vol_sphere_radius)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn vol_spheres_intersect [vol_spheres_intersect_radius_1 vol_spheres_intersect_radius_2 vol_spheres_intersect_centers_distance]
  (binding [vol_spheres_intersect_h1 nil vol_spheres_intersect_h2 nil] (try (do (when (or (or (< vol_spheres_intersect_radius_1 0.0) (< vol_spheres_intersect_radius_2 0.0)) (< vol_spheres_intersect_centers_distance 0.0)) (throw (Exception. "vol_spheres_intersect() only accepts non-negative values"))) (when (= vol_spheres_intersect_centers_distance 0.0) (throw (ex-info "return" {:v (vol_sphere (minf vol_spheres_intersect_radius_1 vol_spheres_intersect_radius_2))}))) (set! vol_spheres_intersect_h1 (/ (* (+ (- vol_spheres_intersect_radius_1 vol_spheres_intersect_radius_2) vol_spheres_intersect_centers_distance) (- (+ vol_spheres_intersect_radius_1 vol_spheres_intersect_radius_2) vol_spheres_intersect_centers_distance)) (* 2.0 vol_spheres_intersect_centers_distance))) (set! vol_spheres_intersect_h2 (/ (* (+ (- vol_spheres_intersect_radius_2 vol_spheres_intersect_radius_1) vol_spheres_intersect_centers_distance) (- (+ vol_spheres_intersect_radius_2 vol_spheres_intersect_radius_1) vol_spheres_intersect_centers_distance)) (* 2.0 vol_spheres_intersect_centers_distance))) (throw (ex-info "return" {:v (+ (vol_spherical_cap vol_spheres_intersect_h1 vol_spheres_intersect_radius_2) (vol_spherical_cap vol_spheres_intersect_h2 vol_spheres_intersect_radius_1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vol_spheres_union [vol_spheres_union_radius_1 vol_spheres_union_radius_2 vol_spheres_union_centers_distance]
  (try (do (when (or (or (<= vol_spheres_union_radius_1 0.0) (<= vol_spheres_union_radius_2 0.0)) (< vol_spheres_union_centers_distance 0.0)) (throw (Exception. "vol_spheres_union() only accepts non-negative values, non-zero radius"))) (if (= vol_spheres_union_centers_distance 0.0) (vol_sphere (maxf vol_spheres_union_radius_1 vol_spheres_union_radius_2)) (- (+ (vol_sphere vol_spheres_union_radius_1) (vol_sphere vol_spheres_union_radius_2)) (vol_spheres_intersect vol_spheres_union_radius_1 vol_spheres_union_radius_2 vol_spheres_union_centers_distance)))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn vol_cuboid [vol_cuboid_width vol_cuboid_height vol_cuboid_length]
  (try (do (when (or (or (< vol_cuboid_width 0.0) (< vol_cuboid_height 0.0)) (< vol_cuboid_length 0.0)) (throw (Exception. "vol_cuboid() only accepts non-negative values"))) (throw (ex-info "return" {:v (* (* vol_cuboid_width vol_cuboid_height) vol_cuboid_length)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn vol_cone [vol_cone_area_of_base vol_cone_height]
  (try (do (when (or (< vol_cone_height 0.0) (< vol_cone_area_of_base 0.0)) (throw (Exception. "vol_cone() only accepts non-negative values"))) (throw (ex-info "return" {:v (/ (* vol_cone_area_of_base vol_cone_height) 3.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn vol_right_circ_cone [vol_right_circ_cone_radius vol_right_circ_cone_height]
  (try (do (when (or (< vol_right_circ_cone_height 0.0) (< vol_right_circ_cone_radius 0.0)) (throw (Exception. "vol_right_circ_cone() only accepts non-negative values"))) (throw (ex-info "return" {:v (/ (* (* (* main_PI vol_right_circ_cone_radius) vol_right_circ_cone_radius) vol_right_circ_cone_height) 3.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn vol_prism [vol_prism_area_of_base vol_prism_height]
  (try (do (when (or (< vol_prism_height 0.0) (< vol_prism_area_of_base 0.0)) (throw (Exception. "vol_prism() only accepts non-negative values"))) (throw (ex-info "return" {:v (* vol_prism_area_of_base vol_prism_height)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn vol_pyramid [vol_pyramid_area_of_base vol_pyramid_height]
  (try (do (when (or (< vol_pyramid_height 0.0) (< vol_pyramid_area_of_base 0.0)) (throw (Exception. "vol_pyramid() only accepts non-negative values"))) (throw (ex-info "return" {:v (/ (* vol_pyramid_area_of_base vol_pyramid_height) 3.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn vol_hemisphere [vol_hemisphere_radius]
  (try (do (when (< vol_hemisphere_radius 0.0) (throw (Exception. "vol_hemisphere() only accepts non-negative values"))) (throw (ex-info "return" {:v (/ (* (* (* (* vol_hemisphere_radius vol_hemisphere_radius) vol_hemisphere_radius) main_PI) 2.0) 3.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn vol_circular_cylinder [vol_circular_cylinder_radius vol_circular_cylinder_height]
  (try (do (when (or (< vol_circular_cylinder_height 0.0) (< vol_circular_cylinder_radius 0.0)) (throw (Exception. "vol_circular_cylinder() only accepts non-negative values"))) (throw (ex-info "return" {:v (* (* (* vol_circular_cylinder_radius vol_circular_cylinder_radius) vol_circular_cylinder_height) main_PI)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn vol_hollow_circular_cylinder [vol_hollow_circular_cylinder_inner_radius vol_hollow_circular_cylinder_outer_radius vol_hollow_circular_cylinder_height]
  (try (do (when (or (or (< vol_hollow_circular_cylinder_inner_radius 0.0) (< vol_hollow_circular_cylinder_outer_radius 0.0)) (< vol_hollow_circular_cylinder_height 0.0)) (throw (Exception. "vol_hollow_circular_cylinder() only accepts non-negative values"))) (when (<= vol_hollow_circular_cylinder_outer_radius vol_hollow_circular_cylinder_inner_radius) (throw (Exception. "outer_radius must be greater than inner_radius"))) (throw (ex-info "return" {:v (* (* main_PI (- (* vol_hollow_circular_cylinder_outer_radius vol_hollow_circular_cylinder_outer_radius) (* vol_hollow_circular_cylinder_inner_radius vol_hollow_circular_cylinder_inner_radius))) vol_hollow_circular_cylinder_height)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn vol_conical_frustum [vol_conical_frustum_height vol_conical_frustum_radius_1 vol_conical_frustum_radius_2]
  (try (do (when (or (or (< vol_conical_frustum_radius_1 0.0) (< vol_conical_frustum_radius_2 0.0)) (< vol_conical_frustum_height 0.0)) (throw (Exception. "vol_conical_frustum() only accepts non-negative values"))) (throw (ex-info "return" {:v (* (* (* (/ 1.0 3.0) main_PI) vol_conical_frustum_height) (+ (+ (* vol_conical_frustum_radius_1 vol_conical_frustum_radius_1) (* vol_conical_frustum_radius_2 vol_conical_frustum_radius_2)) (* vol_conical_frustum_radius_1 vol_conical_frustum_radius_2)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn vol_torus [vol_torus_torus_radius vol_torus_tube_radius]
  (try (do (when (or (< vol_torus_torus_radius 0.0) (< vol_torus_tube_radius 0.0)) (throw (Exception. "vol_torus() only accepts non-negative values"))) (throw (ex-info "return" {:v (* (* (* (* (* 2.0 main_PI) main_PI) vol_torus_torus_radius) vol_torus_tube_radius) vol_torus_tube_radius)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn vol_icosahedron [vol_icosahedron_tri_side]
  (try (do (when (< vol_icosahedron_tri_side 0.0) (throw (Exception. "vol_icosahedron() only accepts non-negative values"))) (throw (ex-info "return" {:v (/ (* (* (* (* vol_icosahedron_tri_side vol_icosahedron_tri_side) vol_icosahedron_tri_side) (+ 3.0 main_SQRT5)) 5.0) 12.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (println "Volumes:") (println (str "Cube: " (str (vol_cube 2.0)))) (println (str "Cuboid: " (str (vol_cuboid 2.0 2.0 2.0)))) (println (str "Cone: " (str (vol_cone 2.0 2.0)))) (println (str "Right Circular Cone: " (str (vol_right_circ_cone 2.0 2.0)))) (println (str "Prism: " (str (vol_prism 2.0 2.0)))) (println (str "Pyramid: " (str (vol_pyramid 2.0 2.0)))) (println (str "Sphere: " (str (vol_sphere 2.0)))) (println (str "Hemisphere: " (str (vol_hemisphere 2.0)))) (println (str "Circular Cylinder: " (str (vol_circular_cylinder 2.0 2.0)))) (println (str "Torus: " (str (vol_torus 2.0 2.0)))) (println (str "Conical Frustum: " (str (vol_conical_frustum 2.0 2.0 4.0)))) (println (str "Spherical cap: " (str (vol_spherical_cap 1.0 2.0)))) (println (str "Spheres intersection: " (str (vol_spheres_intersect 2.0 2.0 1.0)))) (println (str "Spheres union: " (str (vol_spheres_union 2.0 2.0 1.0)))) (println (str "Hollow Circular Cylinder: " (str (vol_hollow_circular_cylinder 1.0 2.0 3.0)))) (println (str "Icosahedron: " (str (vol_icosahedron 2.5))))))

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
