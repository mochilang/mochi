(ns main (:refer-clojure :exclude [parseInt split absf sqrt makeBoolGrid visible computeVis bfs main]))

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

(declare parseInt split absf sqrt makeBoolGrid visible computeVis bfs main)

(declare _read_file)

(def ^:dynamic bfs_c nil)

(def ^:dynamic bfs_d nil)

(def ^:dynamic bfs_diff nil)

(def ^:dynamic bfs_head nil)

(def ^:dynamic bfs_hn nil)

(def ^:dynamic bfs_hr nil)

(def ^:dynamic bfs_idx nil)

(def ^:dynamic bfs_nc nil)

(def ^:dynamic bfs_nr nil)

(def ^:dynamic bfs_qC nil)

(def ^:dynamic bfs_qD nil)

(def ^:dynamic bfs_qR nil)

(def ^:dynamic bfs_r nil)

(def ^:dynamic bfs_vis1 nil)

(def ^:dynamic bfs_vis2 nil)

(def ^:dynamic bfs_visited nil)

(def ^:dynamic computeVis_c nil)

(def ^:dynamic computeVis_r nil)

(def ^:dynamic computeVis_vis nil)

(def ^:dynamic main_C1 nil)

(def ^:dynamic main_C2 nil)

(def ^:dynamic main_P nil)

(def ^:dynamic main_Q nil)

(def ^:dynamic main_R1 nil)

(def ^:dynamic main_R2 nil)

(def ^:dynamic main_c nil)

(def ^:dynamic main_case nil)

(def ^:dynamic main_coords nil)

(def ^:dynamic main_grid nil)

(def ^:dynamic main_line nil)

(def ^:dynamic main_pq nil)

(def ^:dynamic main_r nil)

(def ^:dynamic main_res nil)

(def ^:dynamic main_row nil)

(def ^:dynamic main_rowParts nil)

(def ^:dynamic main_t nil)

(def ^:dynamic main_tLine nil)

(def ^:dynamic makeBoolGrid_g nil)

(def ^:dynamic makeBoolGrid_i nil)

(def ^:dynamic makeBoolGrid_j nil)

(def ^:dynamic makeBoolGrid_row nil)

(def ^:dynamic parseInt_i nil)

(def ^:dynamic parseInt_n nil)

(def ^:dynamic split_ch nil)

(def ^:dynamic split_cur nil)

(def ^:dynamic split_i nil)

(def ^:dynamic split_parts nil)

(def ^:dynamic sqrt_prev nil)

(def ^:dynamic sqrt_r nil)

(def ^:dynamic visible_Dx nil)

(def ^:dynamic visible_Dy nil)

(def ^:dynamic visible_Dz nil)

(def ^:dynamic visible_H nil)

(def ^:dynamic visible_X nil)

(def ^:dynamic visible_X1 nil)

(def ^:dynamic visible_X2 nil)

(def ^:dynamic visible_Y nil)

(def ^:dynamic visible_Y1 nil)

(def ^:dynamic visible_Y2 nil)

(def ^:dynamic visible_Z nil)

(def ^:dynamic visible_Z1 nil)

(def ^:dynamic visible_Z2 nil)

(def ^:dynamic visible_cIdx nil)

(def ^:dynamic visible_dist nil)

(def ^:dynamic visible_i nil)

(def ^:dynamic visible_rIdx nil)

(def ^:dynamic visible_stepT nil)

(def ^:dynamic visible_steps nil)

(def ^:dynamic visible_t nil)

(def ^:dynamic main_digits nil)

(defn parseInt [parseInt_s]
  (binding [parseInt_i nil parseInt_n nil] (try (do (set! parseInt_i 0) (set! parseInt_n 0) (while (< parseInt_i (count parseInt_s)) (do (set! parseInt_n (+' (*' parseInt_n 10) (Long/parseLong (get main_digits (subs parseInt_s parseInt_i (min (+' parseInt_i 1) (count parseInt_s))))))) (set! parseInt_i (+' parseInt_i 1)))) (throw (ex-info "return" {:v parseInt_n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn split [split_s]
  (binding [split_ch nil split_cur nil split_i nil split_parts nil] (try (do (set! split_parts []) (set! split_cur "") (set! split_i 0) (while (< split_i (count split_s)) (do (set! split_ch (subs split_s split_i (min (+' split_i 1) (count split_s)))) (if (= split_ch " ") (when (> (count split_cur) 0) (do (set! split_parts (conj split_parts split_cur)) (set! split_cur ""))) (set! split_cur (str split_cur split_ch))) (set! split_i (+' split_i 1)))) (when (> (count split_cur) 0) (set! split_parts (conj split_parts split_cur))) (throw (ex-info "return" {:v split_parts}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn absf [absf_x]
  (try (if (< absf_x 0.0) (throw (ex-info "return" {:v (- absf_x)})) (throw (ex-info "return" {:v absf_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrt [sqrt_x]
  (binding [sqrt_prev nil sqrt_r nil] (try (do (when (<= sqrt_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrt_r sqrt_x) (set! sqrt_prev 0.0) (while (> (absf (- sqrt_r sqrt_prev)) 0.000000000001) (do (set! sqrt_prev sqrt_r) (set! sqrt_r (/ (+' sqrt_r (/ sqrt_x sqrt_r)) 2.0)))) (throw (ex-info "return" {:v sqrt_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn makeBoolGrid [makeBoolGrid_P makeBoolGrid_Q]
  (binding [makeBoolGrid_g nil makeBoolGrid_i nil makeBoolGrid_j nil makeBoolGrid_row nil] (try (do (set! makeBoolGrid_g []) (set! makeBoolGrid_i 0) (while (< makeBoolGrid_i makeBoolGrid_P) (do (set! makeBoolGrid_row []) (set! makeBoolGrid_j 0) (while (< makeBoolGrid_j makeBoolGrid_Q) (do (set! makeBoolGrid_row (conj makeBoolGrid_row false)) (set! makeBoolGrid_j (+' makeBoolGrid_j 1)))) (set! makeBoolGrid_g (conj makeBoolGrid_g makeBoolGrid_row)) (set! makeBoolGrid_i (+' makeBoolGrid_i 1)))) (throw (ex-info "return" {:v makeBoolGrid_g}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn visible [visible_grid visible_P visible_Q visible_R visible_C visible_BR visible_BC]
  (binding [visible_Dx nil visible_Dy nil visible_Dz nil visible_H nil visible_X nil visible_X1 nil visible_X2 nil visible_Y nil visible_Y1 nil visible_Y2 nil visible_Z nil visible_Z1 nil visible_Z2 nil visible_cIdx nil visible_dist nil visible_i nil visible_rIdx nil visible_stepT nil visible_steps nil visible_t nil] (try (do (set! visible_X1 (- (double visible_C) 0.5)) (set! visible_Y1 (- (double visible_R) 0.5)) (set! visible_Z1 (+' (Double/parseDouble (subs (nth visible_grid (long (- visible_R 1))) (long (- visible_C 1)) (+ (long (- visible_C 1)) 1))) 0.5)) (set! visible_X2 (- (double visible_BC) 0.5)) (set! visible_Y2 (- (double visible_BR) 0.5)) (set! visible_Z2 (+' (Double/parseDouble (subs (nth visible_grid (long (- visible_BR 1))) (long (- visible_BC 1)) (+ (long (- visible_BC 1)) 1))) 0.5)) (set! visible_Dx (- visible_X2 visible_X1)) (set! visible_Dy (- visible_Y2 visible_Y1)) (set! visible_Dz (- visible_Z2 visible_Z1)) (set! visible_dist (sqrt (+' (+' (*' visible_Dx visible_Dx) (*' visible_Dy visible_Dy)) (*' visible_Dz visible_Dz)))) (set! visible_steps (+' (long (*' visible_dist 20.0)) 1)) (set! visible_stepT (/ 1.0 (double visible_steps))) (set! visible_i 1) (while (< visible_i visible_steps) (do (set! visible_t (*' visible_stepT (double visible_i))) (set! visible_X (+' visible_X1 (*' visible_Dx visible_t))) (set! visible_Y (+' visible_Y1 (*' visible_Dy visible_t))) (set! visible_Z (+' visible_Z1 (*' visible_Dz visible_t))) (set! visible_rIdx (+' (long visible_Y) 1)) (set! visible_cIdx (+' (long visible_X) 1)) (when (or (or (or (< visible_rIdx 1) (> visible_rIdx visible_P)) (< visible_cIdx 1)) (> visible_cIdx visible_Q)) (throw (ex-info "return" {:v false}))) (set! visible_H (Double/parseDouble (subs (nth visible_grid (long (- visible_rIdx 1))) (long (- visible_cIdx 1)) (+ (long (- visible_cIdx 1)) 1)))) (when (<= visible_Z visible_H) (throw (ex-info "return" {:v false}))) (set! visible_i (+' visible_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn computeVis [computeVis_grid computeVis_P computeVis_Q computeVis_BR computeVis_BC]
  (binding [computeVis_c nil computeVis_r nil computeVis_vis nil] (try (do (set! computeVis_vis (makeBoolGrid computeVis_P computeVis_Q)) (set! computeVis_r 1) (while (<= computeVis_r computeVis_P) (do (set! computeVis_c 1) (while (<= computeVis_c computeVis_Q) (do (set! computeVis_vis (assoc-in computeVis_vis [(long (- computeVis_r 1)) (long (- computeVis_c 1))] (visible computeVis_grid computeVis_P computeVis_Q computeVis_r computeVis_c computeVis_BR computeVis_BC))) (set! computeVis_c (+' computeVis_c 1)))) (set! computeVis_r (+' computeVis_r 1)))) (throw (ex-info "return" {:v computeVis_vis}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bfs [bfs_grid bfs_P bfs_Q bfs_R1 bfs_C1 bfs_R2 bfs_C2]
  (binding [bfs_c nil bfs_d nil bfs_diff nil bfs_head nil bfs_hn nil bfs_hr nil bfs_idx nil bfs_nc nil bfs_nr nil bfs_qC nil bfs_qD nil bfs_qR nil bfs_r nil bfs_vis1 nil bfs_vis2 nil bfs_visited nil] (try (do (set! bfs_vis1 (computeVis bfs_grid bfs_P bfs_Q bfs_R1 bfs_C1)) (set! bfs_vis2 (computeVis bfs_grid bfs_P bfs_Q bfs_R2 bfs_C2)) (set! bfs_visited (makeBoolGrid bfs_P bfs_Q)) (set! bfs_qR []) (set! bfs_qC []) (set! bfs_qD []) (set! bfs_qR (conj bfs_qR bfs_R1)) (set! bfs_qC (conj bfs_qC bfs_C1)) (set! bfs_qD (conj bfs_qD 0)) (set! bfs_visited (assoc-in bfs_visited [(long (- bfs_R1 1)) (long (- bfs_C1 1))] true)) (set! bfs_head 0) (while (< bfs_head (count bfs_qR)) (do (set! bfs_r (nth bfs_qR bfs_head)) (set! bfs_c (nth bfs_qC bfs_head)) (set! bfs_d (nth bfs_qD bfs_head)) (when (and (= bfs_r bfs_R2) (= bfs_c bfs_C2)) (throw (ex-info "return" {:v bfs_d}))) (set! bfs_hr (subs (nth bfs_grid (long (- bfs_r 1))) (long (- bfs_c 1)) (+ (long (- bfs_c 1)) 1))) (set! bfs_idx 0) (while (< bfs_idx 4) (do (set! bfs_nr bfs_r) (set! bfs_nc bfs_c) (when (= bfs_idx 0) (set! bfs_nr (- bfs_nr 1))) (when (= bfs_idx 1) (set! bfs_nr (str bfs_nr 1))) (when (= bfs_idx 2) (set! bfs_nc (- bfs_nc 1))) (when (= bfs_idx 3) (set! bfs_nc (str bfs_nc 1))) (when (and (and (and (>= (compare bfs_nr 1) 0) (<= (compare bfs_nr bfs_P) 0)) (>= (compare bfs_nc 1) 0)) (<= (compare bfs_nc bfs_Q) 0)) (when (not (subs (nth bfs_visited (long (- bfs_nr 1))) (long (- bfs_nc 1)) (+ (long (- bfs_nc 1)) 1))) (do (set! bfs_hn (subs (nth bfs_grid (long (- bfs_nr 1))) (long (- bfs_nc 1)) (+ (long (- bfs_nc 1)) 1))) (set! bfs_diff (- bfs_hn bfs_hr)) (when (and (<= bfs_diff 1) (>= bfs_diff -3)) (when (or (subs (nth bfs_vis1 (long (- bfs_nr 1))) (long (- bfs_nc 1)) (+ (long (- bfs_nc 1)) 1)) (subs (nth bfs_vis2 (long (- bfs_nr 1))) (long (- bfs_nc 1)) (+ (long (- bfs_nc 1)) 1))) (do (set! bfs_visited (assoc-in bfs_visited [(long (- bfs_nr 1)) (long (- bfs_nc 1))] true)) (set! bfs_qR (conj bfs_qR bfs_nr)) (set! bfs_qC (conj bfs_qC bfs_nc)) (set! bfs_qD (conj bfs_qD (str bfs_d 1))))))))) (set! bfs_idx (+' bfs_idx 1)))) (set! bfs_head (+' bfs_head 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_C1 nil main_C2 nil main_P nil main_Q nil main_R1 nil main_R2 nil main_c nil main_case nil main_coords nil main_grid nil main_line nil main_pq nil main_r nil main_res nil main_row nil main_rowParts nil main_t nil main_tLine nil] (try (do (set! main_tLine (read-line)) (when (= main_tLine "") (throw (ex-info "return" {:v nil}))) (set! main_t (parseInt main_tLine)) (set! main_case 0) (while (< main_case main_t) (do (set! main_line (read-line)) (while (= main_line "") (set! main_line (read-line))) (set! main_pq (split main_line)) (set! main_P (parseInt (nth main_pq 0))) (set! main_Q (parseInt (nth main_pq 1))) (set! main_grid []) (set! main_r 0) (while (< main_r main_P) (do (set! main_rowParts (split (read-line))) (set! main_row []) (set! main_c 0) (while (< main_c main_Q) (do (set! main_row (conj main_row (parseInt (nth main_rowParts main_c)))) (set! main_c (+' main_c 1)))) (set! main_grid (conj main_grid main_row)) (set! main_r (+' main_r 1)))) (set! main_coords (split (read-line))) (set! main_R1 (parseInt (nth main_coords 0))) (set! main_C1 (parseInt (nth main_coords 1))) (set! main_R2 (parseInt (nth main_coords 2))) (set! main_C2 (parseInt (nth main_coords 3))) (set! main_res (bfs main_grid main_P main_Q main_R1 main_C1 main_R2 main_C2)) (if (< main_res 0) (println "Mission impossible!") (println (str (str "The shortest path is " (mochi_str main_res)) " steps long."))) (set! main_case (+' main_case 1))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_digits) (constantly {"0" 0 "1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9}))
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
