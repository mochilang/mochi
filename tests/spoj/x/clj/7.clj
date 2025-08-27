(ns main (:refer-clojure :exclude [split parse_ints sort_unique pointInPoly make3DBool main]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare split parse_ints sort_unique pointInPoly make3DBool main)

(declare _read_file)

(def ^:dynamic main_F nil)

(def ^:dynamic main_P nil)

(def ^:dynamic main_allSame nil)

(def ^:dynamic main_blockX nil)

(def ^:dynamic main_case nil)

(def ^:dynamic main_coord nil)

(def ^:dynamic main_cy nil)

(def ^:dynamic main_cz nil)

(def ^:dynamic main_dx nil)

(def ^:dynamic main_dy nil)

(def ^:dynamic main_dz nil)

(def ^:dynamic main_fLine nil)

(def ^:dynamic main_faceXCoord nil)

(def ^:dynamic main_faceYPoly nil)

(def ^:dynamic main_faceZPoly nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_i2 nil)

(def ^:dynamic main_i3 nil)

(def ^:dynamic main_inside nil)

(def ^:dynamic main_j nil)

(def ^:dynamic main_j2 nil)

(def ^:dynamic main_j3 nil)

(def ^:dynamic main_k nil)

(def ^:dynamic main_k2 nil)

(def ^:dynamic main_k3 nil)

(def ^:dynamic main_line nil)

(def ^:dynamic main_nums nil)

(def ^:dynamic main_nx nil)

(def ^:dynamic main_ny nil)

(def ^:dynamic main_nz nil)

(def ^:dynamic main_polyY nil)

(def ^:dynamic main_polyZ nil)

(def ^:dynamic main_ptsX nil)

(def ^:dynamic main_ptsY nil)

(def ^:dynamic main_ptsZ nil)

(def ^:dynamic main_solid nil)

(def ^:dynamic main_t nil)

(def ^:dynamic main_tLine nil)

(def ^:dynamic main_volume nil)

(def ^:dynamic main_x nil)

(def ^:dynamic main_xIndex nil)

(def ^:dynamic main_xi nil)

(def ^:dynamic main_xs nil)

(def ^:dynamic main_y nil)

(def ^:dynamic main_ys nil)

(def ^:dynamic main_z nil)

(def ^:dynamic main_zs nil)

(def ^:dynamic make3DBool_arr nil)

(def ^:dynamic make3DBool_i nil)

(def ^:dynamic make3DBool_j nil)

(def ^:dynamic make3DBool_k nil)

(def ^:dynamic make3DBool_plane nil)

(def ^:dynamic make3DBool_row nil)

(def ^:dynamic parse_ints_i nil)

(def ^:dynamic parse_ints_nums nil)

(def ^:dynamic parse_ints_p nil)

(def ^:dynamic parse_ints_pieces nil)

(def ^:dynamic pointInPoly_i nil)

(def ^:dynamic pointInPoly_inside nil)

(def ^:dynamic pointInPoly_j nil)

(def ^:dynamic pointInPoly_xi nil)

(def ^:dynamic pointInPoly_xint nil)

(def ^:dynamic pointInPoly_xj nil)

(def ^:dynamic pointInPoly_yi nil)

(def ^:dynamic pointInPoly_yj nil)

(def ^:dynamic sort_unique_arr nil)

(def ^:dynamic sort_unique_i nil)

(def ^:dynamic sort_unique_j nil)

(def ^:dynamic sort_unique_res nil)

(def ^:dynamic sort_unique_tmp nil)

(def ^:dynamic split_cur nil)

(def ^:dynamic split_i nil)

(def ^:dynamic split_parts nil)

(defn split [split_s split_sep]
  (binding [split_cur nil split_i nil split_parts nil] (try (do (set! split_parts []) (set! split_cur "") (set! split_i 0) (while (< split_i (count split_s)) (if (and (and (> (count split_sep) 0) (<= (+' split_i (count split_sep)) (count split_s))) (= (subs split_s split_i (min (+' split_i (count split_sep)) (count split_s))) split_sep)) (do (set! split_parts (conj split_parts split_cur)) (set! split_cur "") (set! split_i (+' split_i (count split_sep)))) (do (set! split_cur (str split_cur (subs split_s split_i (min (+' split_i 1) (count split_s))))) (set! split_i (+' split_i 1))))) (set! split_parts (conj split_parts split_cur)) (throw (ex-info "return" {:v split_parts}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn parse_ints [parse_ints_line]
  (binding [parse_ints_i nil parse_ints_nums nil parse_ints_p nil parse_ints_pieces nil] (try (do (set! parse_ints_pieces (split parse_ints_line " ")) (set! parse_ints_nums []) (set! parse_ints_i 0) (while (< parse_ints_i (count parse_ints_pieces)) (do (set! parse_ints_p (nth parse_ints_pieces parse_ints_i)) (when (> (count parse_ints_p) 0) (set! parse_ints_nums (conj parse_ints_nums (long parse_ints_p)))) (set! parse_ints_i (+' parse_ints_i 1)))) (throw (ex-info "return" {:v parse_ints_nums}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sort_unique [sort_unique_arr_p]
  (binding [sort_unique_arr sort_unique_arr_p sort_unique_i nil sort_unique_j nil sort_unique_res nil sort_unique_tmp nil] (try (do (set! sort_unique_i 1) (while (< sort_unique_i (count sort_unique_arr)) (do (set! sort_unique_j sort_unique_i) (while (and (> sort_unique_j 0) (> (compare (nth sort_unique_arr (- sort_unique_j 1)) (nth sort_unique_arr sort_unique_j)) 0)) (do (set! sort_unique_tmp (nth sort_unique_arr (- sort_unique_j 1))) (set! sort_unique_arr (assoc sort_unique_arr (- sort_unique_j 1) (nth sort_unique_arr sort_unique_j))) (set! sort_unique_arr (assoc sort_unique_arr sort_unique_j sort_unique_tmp)) (set! sort_unique_j (- sort_unique_j 1)))) (set! sort_unique_i (+' sort_unique_i 1)))) (set! sort_unique_res []) (set! sort_unique_i 0) (while (< sort_unique_i (count sort_unique_arr)) (do (when (or (= sort_unique_i 0) (not= (subs sort_unique_arr sort_unique_i (+ sort_unique_i 1)) (subs sort_unique_arr (- sort_unique_i 1) (+ (- sort_unique_i 1) 1)))) (set! sort_unique_res (conj sort_unique_res (subs sort_unique_arr sort_unique_i (+ sort_unique_i 1))))) (set! sort_unique_i (+' sort_unique_i 1)))) (throw (ex-info "return" {:v sort_unique_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var sort_unique_arr) (constantly sort_unique_arr))))))

(defn pointInPoly [pointInPoly_xs pointInPoly_ys pointInPoly_px pointInPoly_py]
  (binding [pointInPoly_i nil pointInPoly_inside nil pointInPoly_j nil pointInPoly_xi nil pointInPoly_xint nil pointInPoly_xj nil pointInPoly_yi nil pointInPoly_yj nil] (try (do (set! pointInPoly_inside false) (set! pointInPoly_i 0) (set! pointInPoly_j (- (count pointInPoly_xs) 1)) (while (< pointInPoly_i (count pointInPoly_xs)) (do (set! pointInPoly_xi (Double/parseDouble (nth pointInPoly_xs pointInPoly_i))) (set! pointInPoly_yi (Double/parseDouble (nth pointInPoly_ys pointInPoly_i))) (set! pointInPoly_xj (Double/parseDouble (nth pointInPoly_xs pointInPoly_j))) (set! pointInPoly_yj (Double/parseDouble (nth pointInPoly_ys pointInPoly_j))) (when (or (and (> pointInPoly_yi pointInPoly_py) (<= pointInPoly_yj pointInPoly_py)) (and (> pointInPoly_yj pointInPoly_py) (<= pointInPoly_yi pointInPoly_py))) (do (set! pointInPoly_xint (+' (/ (*' (- pointInPoly_xj pointInPoly_xi) (- pointInPoly_py pointInPoly_yi)) (- pointInPoly_yj pointInPoly_yi)) pointInPoly_xi)) (when (< pointInPoly_px pointInPoly_xint) (set! pointInPoly_inside (not pointInPoly_inside))))) (set! pointInPoly_j pointInPoly_i) (set! pointInPoly_i (+' pointInPoly_i 1)))) (throw (ex-info "return" {:v pointInPoly_inside}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn make3DBool [make3DBool_a make3DBool_b make3DBool_c]
  (binding [make3DBool_arr nil make3DBool_i nil make3DBool_j nil make3DBool_k nil make3DBool_plane nil make3DBool_row nil] (try (do (set! make3DBool_arr []) (set! make3DBool_i 0) (while (< make3DBool_i make3DBool_a) (do (set! make3DBool_plane []) (set! make3DBool_j 0) (while (< make3DBool_j make3DBool_b) (do (set! make3DBool_row []) (set! make3DBool_k 0) (while (< make3DBool_k make3DBool_c) (do (set! make3DBool_row (conj make3DBool_row false)) (set! make3DBool_k (+' make3DBool_k 1)))) (set! make3DBool_plane (conj make3DBool_plane make3DBool_row)) (set! make3DBool_j (+' make3DBool_j 1)))) (set! make3DBool_arr (conj make3DBool_arr make3DBool_plane)) (set! make3DBool_i (+' make3DBool_i 1)))) (throw (ex-info "return" {:v make3DBool_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_F nil main_P nil main_allSame nil main_blockX nil main_case nil main_coord nil main_cy nil main_cz nil main_dx nil main_dy nil main_dz nil main_fLine nil main_faceXCoord nil main_faceYPoly nil main_faceZPoly nil main_i nil main_i2 nil main_i3 nil main_inside nil main_j nil main_j2 nil main_j3 nil main_k nil main_k2 nil main_k3 nil main_line nil main_nums nil main_nx nil main_ny nil main_nz nil main_polyY nil main_polyZ nil main_ptsX nil main_ptsY nil main_ptsZ nil main_solid nil main_t nil main_tLine nil main_volume nil main_x nil main_xIndex nil main_xi nil main_xs nil main_y nil main_ys nil main_z nil main_zs nil] (try (do (set! main_tLine (read-line)) (when (= main_tLine "") (throw (ex-info "return" {:v nil}))) (set! main_t (Long/parseLong main_tLine)) (set! main_case 0) (while (< main_case main_t) (do (set! main_fLine (read-line)) (set! main_F (Long/parseLong main_fLine)) (set! main_xs []) (set! main_ys []) (set! main_zs []) (set! main_xs (conj main_xs 0)) (set! main_xs (conj main_xs 1001)) (set! main_ys (conj main_ys 0)) (set! main_ys (conj main_ys 1001)) (set! main_zs (conj main_zs 0)) (set! main_zs (conj main_zs 1001)) (set! main_faceXCoord []) (set! main_faceYPoly []) (set! main_faceZPoly []) (set! main_i 0) (while (< main_i main_F) (do (set! main_line (read-line)) (set! main_nums (parse_ints main_line)) (set! main_P (nth main_nums 0)) (set! main_ptsX []) (set! main_ptsY []) (set! main_ptsZ []) (set! main_j 0) (while (< (compare main_j main_P) 0) (do (set! main_x (nth main_nums (+' 1 (*' 3 main_j)))) (set! main_y (nth main_nums (+' (+' 1 (*' 3 main_j)) 1))) (set! main_z (nth main_nums (+' (+' 1 (*' 3 main_j)) 2))) (set! main_ptsX (conj main_ptsX main_x)) (set! main_ptsY (conj main_ptsY main_y)) (set! main_ptsZ (conj main_ptsZ main_z)) (set! main_xs (conj main_xs main_x)) (set! main_ys (conj main_ys main_y)) (set! main_zs (conj main_zs main_z)) (set! main_j (+' main_j 1)))) (set! main_allSame true) (set! main_j 1) (while (< (compare main_j main_P) 0) (do (when (not= (nth main_ptsX main_j) (nth main_ptsX 0)) (set! main_allSame false)) (set! main_j (+' main_j 1)))) (when main_allSame (do (set! main_faceXCoord (conj main_faceXCoord (nth main_ptsX 0))) (set! main_faceYPoly (conj main_faceYPoly main_ptsY)) (set! main_faceZPoly (conj main_faceZPoly main_ptsZ)))) (set! main_i (+' main_i 1)))) (set! main_xs (let [__res (sort_unique main_xs)] (do (set! main_xs sort_unique_arr) __res))) (set! main_ys (let [__res (sort_unique main_ys)] (do (set! main_ys sort_unique_arr) __res))) (set! main_zs (let [__res (sort_unique main_zs)] (do (set! main_zs sort_unique_arr) __res))) (set! main_nx (- (count main_xs) 1)) (set! main_ny (- (count main_ys) 1)) (set! main_nz (- (count main_zs) 1)) (set! main_xIndex {}) (set! main_i 0) (while (< main_i (count main_xs)) (do (set! main_xIndex (assoc main_xIndex (nth main_xs main_i) main_i)) (set! main_i (+' main_i 1)))) (set! main_dx []) (set! main_i 0) (while (< main_i main_nx) (do (set! main_dx (conj main_dx (- (nth main_xs (+' main_i 1)) (nth main_xs main_i)))) (set! main_i (+' main_i 1)))) (set! main_dy []) (set! main_i 0) (while (< main_i main_ny) (do (set! main_dy (conj main_dy (- (nth main_ys (+' main_i 1)) (nth main_ys main_i)))) (set! main_i (+' main_i 1)))) (set! main_dz []) (set! main_i 0) (while (< main_i main_nz) (do (set! main_dz (conj main_dz (- (nth main_zs (+' main_i 1)) (nth main_zs main_i)))) (set! main_i (+' main_i 1)))) (set! main_blockX (make3DBool (count main_xs) main_ny main_nz)) (set! main_i 0) (while (< main_i (count main_faceXCoord)) (do (set! main_coord (nth main_faceXCoord main_i)) (set! main_polyY (nth main_faceYPoly main_i)) (set! main_polyZ (nth main_faceZPoly main_i)) (set! main_xi (get main_xIndex main_coord)) (set! main_j 0) (while (< main_j main_ny) (do (set! main_cy (/ (Double/parseDouble (str (nth main_ys main_j) (nth main_ys (+' main_j 1)))) 2.0)) (set! main_k 0) (while (< main_k main_nz) (do (set! main_cz (/ (Double/parseDouble (str (nth main_zs main_k) (nth main_zs (+' main_k 1)))) 2.0)) (when (pointInPoly main_polyY main_polyZ main_cy main_cz) (set! main_blockX (assoc-in main_blockX [main_xi main_j main_k] true))) (set! main_k (+' main_k 1)))) (set! main_j (+' main_j 1)))) (set! main_i (+' main_i 1)))) (set! main_solid (make3DBool main_nx main_ny main_nz)) (set! main_j2 0) (while (< main_j2 main_ny) (do (set! main_k2 0) (while (< main_k2 main_nz) (do (set! main_inside false) (set! main_i2 0) (while (< main_i2 main_nx) (do (when (subs (subs (nth main_blockX main_i2) main_j2 (+ main_j2 1)) main_k2 (+ main_k2 1)) (set! main_inside (not main_inside))) (when main_inside (set! main_solid (assoc-in main_solid [main_i2 main_j2 main_k2] true))) (set! main_i2 (+' main_i2 1)))) (set! main_k2 (+' main_k2 1)))) (set! main_j2 (+' main_j2 1)))) (set! main_volume 0) (set! main_i3 0) (while (< main_i3 main_nx) (do (set! main_j3 0) (while (< main_j3 main_ny) (do (set! main_k3 0) (while (< main_k3 main_nz) (do (when (subs (subs (nth main_solid main_i3) main_j3 (+ main_j3 1)) main_k3 (+ main_k3 1)) (set! main_volume (+' main_volume (*' (*' (nth main_dx main_i3) (nth main_dy main_j3)) (nth main_dz main_k3))))) (set! main_k3 (+' main_k3 1)))) (set! main_j3 (+' main_j3 1)))) (set! main_i3 (+' main_i3 1)))) (println (str (str "The bulk is composed of " (mochi_str main_volume)) " units.")) (set! main_case (+' main_case 1))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

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
