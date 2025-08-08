(ns main (:refer-clojure :exclude [rand_float hypercube_points build_kdtree distance_sq nearest_neighbour_search test_build_cases test_search test_edge main]))

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

(declare rand_float hypercube_points build_kdtree distance_sq nearest_neighbour_search test_build_cases test_search test_edge main)

(def ^:dynamic distance_sq_d nil)

(def ^:dynamic distance_sq_i nil)

(def ^:dynamic distance_sq_sum nil)

(def ^:dynamic hypercube_points_i nil)

(def ^:dynamic hypercube_points_j nil)

(def ^:dynamic hypercube_points_p nil)

(def ^:dynamic hypercube_points_pts nil)

(def ^:dynamic hypercube_points_v nil)

(def ^:dynamic nearest_neighbour_search_d nil)

(def ^:dynamic nearest_neighbour_search_i nil)

(def ^:dynamic nearest_neighbour_search_nearest_dist nil)

(def ^:dynamic nearest_neighbour_search_nearest_idx nil)

(def ^:dynamic nearest_neighbour_search_visited nil)

(def ^:dynamic test_build_cases_empty_pts nil)

(def ^:dynamic test_build_cases_pts1 nil)

(def ^:dynamic test_build_cases_pts2 nil)

(def ^:dynamic test_build_cases_tree0 nil)

(def ^:dynamic test_build_cases_tree1 nil)

(def ^:dynamic test_build_cases_tree2 nil)

(def ^:dynamic test_edge_empty_pts nil)

(def ^:dynamic test_edge_query nil)

(def ^:dynamic test_edge_res nil)

(def ^:dynamic test_edge_tree nil)

(def ^:dynamic test_search_pts nil)

(def ^:dynamic test_search_qp nil)

(def ^:dynamic test_search_res nil)

(def ^:dynamic test_search_tree nil)

(def ^:dynamic main_INF 1000000000.0)

(def ^:dynamic main_seed 1)

(defn rand_float []
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* main_seed 1103515245) 12345) 2147483648))) (throw (ex-info "return" {:v (/ (double main_seed) 2147483648.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn hypercube_points [hypercube_points_num_points hypercube_points_cube_size hypercube_points_num_dimensions]
  (binding [hypercube_points_i nil hypercube_points_j nil hypercube_points_p nil hypercube_points_pts nil hypercube_points_v nil] (try (do (set! hypercube_points_pts []) (set! hypercube_points_i 0) (while (< hypercube_points_i hypercube_points_num_points) (do (set! hypercube_points_p []) (set! hypercube_points_j 0) (while (< hypercube_points_j hypercube_points_num_dimensions) (do (set! hypercube_points_v (* hypercube_points_cube_size (rand_float))) (set! hypercube_points_p (conj hypercube_points_p hypercube_points_v)) (set! hypercube_points_j (+ hypercube_points_j 1)))) (set! hypercube_points_pts (conj hypercube_points_pts hypercube_points_p)) (set! hypercube_points_i (+ hypercube_points_i 1)))) (throw (ex-info "return" {:v hypercube_points_pts}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn build_kdtree [build_kdtree_points build_kdtree_depth]
  (try (throw (ex-info "return" {:v build_kdtree_points})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn distance_sq [distance_sq_a distance_sq_b]
  (binding [distance_sq_d nil distance_sq_i nil distance_sq_sum nil] (try (do (set! distance_sq_sum 0.0) (set! distance_sq_i 0) (while (< distance_sq_i (count distance_sq_a)) (do (set! distance_sq_d (- (nth distance_sq_a distance_sq_i) (nth distance_sq_b distance_sq_i))) (set! distance_sq_sum (+ distance_sq_sum (* distance_sq_d distance_sq_d))) (set! distance_sq_i (+ distance_sq_i 1)))) (throw (ex-info "return" {:v distance_sq_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn nearest_neighbour_search [nearest_neighbour_search_points nearest_neighbour_search_query]
  (binding [nearest_neighbour_search_d nil nearest_neighbour_search_i nil nearest_neighbour_search_nearest_dist nil nearest_neighbour_search_nearest_idx nil nearest_neighbour_search_visited nil] (try (do (when (= (count nearest_neighbour_search_points) 0) (throw (ex-info "return" {:v {"dist" main_INF "index" (- 1.0) "visited" 0.0}}))) (set! nearest_neighbour_search_nearest_idx 0) (set! nearest_neighbour_search_nearest_dist main_INF) (set! nearest_neighbour_search_visited 0) (set! nearest_neighbour_search_i 0) (while (< nearest_neighbour_search_i (count nearest_neighbour_search_points)) (do (set! nearest_neighbour_search_d (distance_sq nearest_neighbour_search_query (nth nearest_neighbour_search_points nearest_neighbour_search_i))) (set! nearest_neighbour_search_visited (+ nearest_neighbour_search_visited 1)) (when (< nearest_neighbour_search_d nearest_neighbour_search_nearest_dist) (do (set! nearest_neighbour_search_nearest_dist nearest_neighbour_search_d) (set! nearest_neighbour_search_nearest_idx nearest_neighbour_search_i))) (set! nearest_neighbour_search_i (+ nearest_neighbour_search_i 1)))) (throw (ex-info "return" {:v {"dist" nearest_neighbour_search_nearest_dist "index" (double nearest_neighbour_search_nearest_idx) "visited" (double nearest_neighbour_search_visited)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_build_cases []
  (binding [test_build_cases_empty_pts nil test_build_cases_pts1 nil test_build_cases_pts2 nil test_build_cases_tree0 nil test_build_cases_tree1 nil test_build_cases_tree2 nil] (do (set! test_build_cases_empty_pts []) (set! test_build_cases_tree0 (build_kdtree test_build_cases_empty_pts 0)) (if (= (count test_build_cases_tree0) 0) (println "case1 true") (println "case1 false")) (set! test_build_cases_pts1 (hypercube_points 10 10.0 2)) (set! test_build_cases_tree1 (build_kdtree test_build_cases_pts1 2)) (if (and (> (count test_build_cases_tree1) 0) (= (count (nth test_build_cases_tree1 0)) 2)) (println "case2 true") (println "case2 false")) (set! test_build_cases_pts2 (hypercube_points 10 10.0 3)) (set! test_build_cases_tree2 (build_kdtree test_build_cases_pts2 (- 2))) (if (and (> (count test_build_cases_tree2) 0) (= (count (nth test_build_cases_tree2 0)) 3)) (println "case3 true") (println "case3 false")))))

(defn test_search []
  (binding [test_search_pts nil test_search_qp nil test_search_res nil test_search_tree nil] (do (set! test_search_pts (hypercube_points 10 10.0 2)) (set! test_search_tree (build_kdtree test_search_pts 0)) (set! test_search_qp (nth (hypercube_points 1 10.0 2) 0)) (set! test_search_res (nearest_neighbour_search test_search_tree test_search_qp)) (if (and (and (not= (get test_search_res "index") (- 1.0)) (>= (get test_search_res "dist") 0.0)) (> (get test_search_res "visited") 0.0)) (println "search true") (println "search false")))))

(defn test_edge []
  (binding [test_edge_empty_pts nil test_edge_query nil test_edge_res nil test_edge_tree nil] (do (set! test_edge_empty_pts []) (set! test_edge_tree (build_kdtree test_edge_empty_pts 0)) (set! test_edge_query [0.0 0.0]) (set! test_edge_res (nearest_neighbour_search test_edge_tree test_edge_query)) (if (and (and (= (get test_edge_res "index") (- 1.0)) (> (get test_edge_res "dist") 100000000.0)) (= (get test_edge_res "visited") 0.0)) (println "edge true") (println "edge false")))))

(defn main []
  (do (alter-var-root (var main_seed) (fn [_] 1)) (test_build_cases) (test_search) (test_edge)))

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
