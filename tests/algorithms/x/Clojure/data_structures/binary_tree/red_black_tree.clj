(ns main (:refer-clojure :exclude [make_tree rotate_left rotate_right insert_fix tree_insert inorder main]))

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

(declare make_tree rotate_left rotate_right insert_fix tree_insert inorder main)

(declare _read_file)

(def ^:dynamic inorder_acc nil)

(def ^:dynamic insert_fix_gp nil)

(def ^:dynamic insert_fix_nodes nil)

(def ^:dynamic insert_fix_t nil)

(def ^:dynamic insert_fix_y nil)

(def ^:dynamic insert_fix_z nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_res nil)

(def ^:dynamic main_t nil)

(def ^:dynamic main_values nil)

(def ^:dynamic rotate_left_nodes nil)

(def ^:dynamic rotate_left_t nil)

(def ^:dynamic rotate_left_xParent nil)

(def ^:dynamic rotate_left_y nil)

(def ^:dynamic rotate_left_yLeft nil)

(def ^:dynamic rotate_right_nodes nil)

(def ^:dynamic rotate_right_t nil)

(def ^:dynamic rotate_right_xParent nil)

(def ^:dynamic rotate_right_y nil)

(def ^:dynamic rotate_right_yRight nil)

(def ^:dynamic tree_insert_idx nil)

(def ^:dynamic tree_insert_node nil)

(def ^:dynamic tree_insert_nodes nil)

(def ^:dynamic tree_insert_t nil)

(def ^:dynamic tree_insert_x nil)

(def ^:dynamic tree_insert_y nil)

(def ^:dynamic main_LABEL nil)

(def ^:dynamic main_COLOR nil)

(def ^:dynamic main_PARENT nil)

(def ^:dynamic main_LEFT nil)

(def ^:dynamic main_RIGHT nil)

(def ^:dynamic main_NEG_ONE nil)

(defn make_tree []
  (try (throw (ex-info "return" {:v {:nodes [] :root (- 1)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rotate_left [rotate_left_t_p rotate_left_x]
  (binding [rotate_left_t rotate_left_t_p rotate_left_nodes nil rotate_left_xParent nil rotate_left_y nil rotate_left_yLeft nil] (try (do (set! rotate_left_nodes (:nodes rotate_left_t)) (set! rotate_left_y (get (get rotate_left_nodes rotate_left_x) main_RIGHT)) (set! rotate_left_yLeft (get (get rotate_left_nodes rotate_left_y) main_LEFT)) (set! rotate_left_nodes (assoc-in rotate_left_nodes [rotate_left_x main_RIGHT] rotate_left_yLeft)) (when (not= rotate_left_yLeft main_NEG_ONE) (set! rotate_left_nodes (assoc-in rotate_left_nodes [rotate_left_yLeft main_PARENT] rotate_left_x))) (set! rotate_left_xParent (get (get rotate_left_nodes rotate_left_x) main_PARENT)) (set! rotate_left_nodes (assoc-in rotate_left_nodes [rotate_left_y main_PARENT] rotate_left_xParent)) (if (= rotate_left_xParent main_NEG_ONE) (set! rotate_left_t (assoc rotate_left_t :root rotate_left_y)) (if (= rotate_left_x (get (get rotate_left_nodes rotate_left_xParent) main_LEFT)) (set! rotate_left_nodes (assoc-in rotate_left_nodes [rotate_left_xParent main_LEFT] rotate_left_y)) (set! rotate_left_nodes (assoc-in rotate_left_nodes [rotate_left_xParent main_RIGHT] rotate_left_y)))) (set! rotate_left_nodes (assoc-in rotate_left_nodes [rotate_left_y main_LEFT] rotate_left_x)) (set! rotate_left_nodes (assoc-in rotate_left_nodes [rotate_left_x main_PARENT] rotate_left_y)) (set! rotate_left_t (assoc rotate_left_t :nodes rotate_left_nodes)) (throw (ex-info "return" {:v rotate_left_t}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var rotate_left_t) (constantly rotate_left_t))))))

(defn rotate_right [rotate_right_t_p rotate_right_x]
  (binding [rotate_right_t rotate_right_t_p rotate_right_nodes nil rotate_right_xParent nil rotate_right_y nil rotate_right_yRight nil] (try (do (set! rotate_right_nodes (:nodes rotate_right_t)) (set! rotate_right_y (get (get rotate_right_nodes rotate_right_x) main_LEFT)) (set! rotate_right_yRight (get (get rotate_right_nodes rotate_right_y) main_RIGHT)) (set! rotate_right_nodes (assoc-in rotate_right_nodes [rotate_right_x main_LEFT] rotate_right_yRight)) (when (not= rotate_right_yRight main_NEG_ONE) (set! rotate_right_nodes (assoc-in rotate_right_nodes [rotate_right_yRight main_PARENT] rotate_right_x))) (set! rotate_right_xParent (get (get rotate_right_nodes rotate_right_x) main_PARENT)) (set! rotate_right_nodes (assoc-in rotate_right_nodes [rotate_right_y main_PARENT] rotate_right_xParent)) (if (= rotate_right_xParent main_NEG_ONE) (set! rotate_right_t (assoc rotate_right_t :root rotate_right_y)) (if (= rotate_right_x (get (get rotate_right_nodes rotate_right_xParent) main_RIGHT)) (set! rotate_right_nodes (assoc-in rotate_right_nodes [rotate_right_xParent main_RIGHT] rotate_right_y)) (set! rotate_right_nodes (assoc-in rotate_right_nodes [rotate_right_xParent main_LEFT] rotate_right_y)))) (set! rotate_right_nodes (assoc-in rotate_right_nodes [rotate_right_y main_RIGHT] rotate_right_x)) (set! rotate_right_nodes (assoc-in rotate_right_nodes [rotate_right_x main_PARENT] rotate_right_y)) (set! rotate_right_t (assoc rotate_right_t :nodes rotate_right_nodes)) (throw (ex-info "return" {:v rotate_right_t}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var rotate_right_t) (constantly rotate_right_t))))))

(defn insert_fix [insert_fix_t_p insert_fix_z_p]
  (binding [insert_fix_t insert_fix_t_p insert_fix_z insert_fix_z_p insert_fix_gp nil insert_fix_nodes nil insert_fix_y nil] (try (do (set! insert_fix_nodes (:nodes insert_fix_t)) (while (and (not= insert_fix_z (:root insert_fix_t)) (= (get (get insert_fix_nodes (get (get insert_fix_nodes insert_fix_z) main_PARENT)) main_COLOR) 1)) (if (= (get (get insert_fix_nodes insert_fix_z) main_PARENT) (get (get insert_fix_nodes (get (get insert_fix_nodes (get (get insert_fix_nodes insert_fix_z) main_PARENT)) main_PARENT)) main_LEFT)) (do (set! insert_fix_y (get (get insert_fix_nodes (get (get insert_fix_nodes (get (get insert_fix_nodes insert_fix_z) main_PARENT)) main_PARENT)) main_RIGHT)) (if (and (not= insert_fix_y main_NEG_ONE) (= (get (get insert_fix_nodes insert_fix_y) main_COLOR) 1)) (do (set! insert_fix_nodes (assoc-in insert_fix_nodes [(get (get insert_fix_nodes insert_fix_z) main_PARENT) main_COLOR] 0)) (set! insert_fix_nodes (assoc-in insert_fix_nodes [insert_fix_y main_COLOR] 0)) (set! insert_fix_gp (get (get insert_fix_nodes (get (get insert_fix_nodes insert_fix_z) main_PARENT)) main_PARENT)) (set! insert_fix_nodes (assoc-in insert_fix_nodes [insert_fix_gp main_COLOR] 1)) (set! insert_fix_z insert_fix_gp)) (do (when (= insert_fix_z (get (get insert_fix_nodes (get (get insert_fix_nodes insert_fix_z) main_PARENT)) main_RIGHT)) (do (set! insert_fix_z (get (get insert_fix_nodes insert_fix_z) main_PARENT)) (set! insert_fix_t (assoc insert_fix_t :nodes insert_fix_nodes)) (set! insert_fix_t (let [__res (rotate_left insert_fix_t insert_fix_z)] (do (set! insert_fix_t rotate_left_t) __res))) (set! insert_fix_nodes (:nodes insert_fix_t)))) (set! insert_fix_nodes (assoc-in insert_fix_nodes [(get (get insert_fix_nodes insert_fix_z) main_PARENT) main_COLOR] 0)) (set! insert_fix_gp (get (get insert_fix_nodes (get (get insert_fix_nodes insert_fix_z) main_PARENT)) main_PARENT)) (set! insert_fix_nodes (assoc-in insert_fix_nodes [insert_fix_gp main_COLOR] 1)) (set! insert_fix_t (assoc insert_fix_t :nodes insert_fix_nodes)) (set! insert_fix_t (let [__res (rotate_right insert_fix_t insert_fix_gp)] (do (set! insert_fix_t rotate_right_t) __res))) (set! insert_fix_nodes (:nodes insert_fix_t))))) (do (set! insert_fix_y (get (get insert_fix_nodes (get (get insert_fix_nodes (get (get insert_fix_nodes insert_fix_z) main_PARENT)) main_PARENT)) main_LEFT)) (if (and (not= insert_fix_y main_NEG_ONE) (= (get (get insert_fix_nodes insert_fix_y) main_COLOR) 1)) (do (set! insert_fix_nodes (assoc-in insert_fix_nodes [(get (get insert_fix_nodes insert_fix_z) main_PARENT) main_COLOR] 0)) (set! insert_fix_nodes (assoc-in insert_fix_nodes [insert_fix_y main_COLOR] 0)) (set! insert_fix_gp (get (get insert_fix_nodes (get (get insert_fix_nodes insert_fix_z) main_PARENT)) main_PARENT)) (set! insert_fix_nodes (assoc-in insert_fix_nodes [insert_fix_gp main_COLOR] 1)) (set! insert_fix_z insert_fix_gp)) (do (when (= insert_fix_z (get (get insert_fix_nodes (get (get insert_fix_nodes insert_fix_z) main_PARENT)) main_LEFT)) (do (set! insert_fix_z (get (get insert_fix_nodes insert_fix_z) main_PARENT)) (set! insert_fix_t (assoc insert_fix_t :nodes insert_fix_nodes)) (set! insert_fix_t (let [__res (rotate_right insert_fix_t insert_fix_z)] (do (set! insert_fix_t rotate_right_t) __res))) (set! insert_fix_nodes (:nodes insert_fix_t)))) (set! insert_fix_nodes (assoc-in insert_fix_nodes [(get (get insert_fix_nodes insert_fix_z) main_PARENT) main_COLOR] 0)) (set! insert_fix_gp (get (get insert_fix_nodes (get (get insert_fix_nodes insert_fix_z) main_PARENT)) main_PARENT)) (set! insert_fix_nodes (assoc-in insert_fix_nodes [insert_fix_gp main_COLOR] 1)) (set! insert_fix_t (assoc insert_fix_t :nodes insert_fix_nodes)) (set! insert_fix_t (let [__res (rotate_left insert_fix_t insert_fix_gp)] (do (set! insert_fix_t rotate_left_t) __res))) (set! insert_fix_nodes (:nodes insert_fix_t))))))) (set! insert_fix_nodes (:nodes insert_fix_t)) (set! insert_fix_nodes (assoc-in insert_fix_nodes [(:root insert_fix_t) main_COLOR] 0)) (set! insert_fix_t (assoc insert_fix_t :nodes insert_fix_nodes)) (throw (ex-info "return" {:v insert_fix_t}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var insert_fix_t) (constantly insert_fix_t)) (alter-var-root (var insert_fix_z) (constantly insert_fix_z))))))

(defn tree_insert [tree_insert_t_p tree_insert_v]
  (binding [tree_insert_t tree_insert_t_p tree_insert_idx nil tree_insert_node nil tree_insert_nodes nil tree_insert_x nil tree_insert_y nil] (try (do (set! tree_insert_nodes (:nodes tree_insert_t)) (set! tree_insert_node [tree_insert_v 1 (- 1) (- 1) (- 1)]) (set! tree_insert_nodes (conj tree_insert_nodes tree_insert_node)) (set! tree_insert_idx (- (count tree_insert_nodes) 1)) (set! tree_insert_y main_NEG_ONE) (set! tree_insert_x (:root tree_insert_t)) (while (not= tree_insert_x main_NEG_ONE) (do (set! tree_insert_y tree_insert_x) (if (< tree_insert_v (get (get tree_insert_nodes tree_insert_x) main_LABEL)) (set! tree_insert_x (get (get tree_insert_nodes tree_insert_x) main_LEFT)) (set! tree_insert_x (get (get tree_insert_nodes tree_insert_x) main_RIGHT))))) (set! tree_insert_nodes (assoc-in tree_insert_nodes [tree_insert_idx main_PARENT] tree_insert_y)) (if (= tree_insert_y main_NEG_ONE) (set! tree_insert_t (assoc tree_insert_t :root tree_insert_idx)) (if (< tree_insert_v (get (get tree_insert_nodes tree_insert_y) main_LABEL)) (set! tree_insert_nodes (assoc-in tree_insert_nodes [tree_insert_y main_LEFT] tree_insert_idx)) (set! tree_insert_nodes (assoc-in tree_insert_nodes [tree_insert_y main_RIGHT] tree_insert_idx)))) (set! tree_insert_t (assoc tree_insert_t :nodes tree_insert_nodes)) (set! tree_insert_t (let [__res (insert_fix tree_insert_t tree_insert_idx)] (do (set! tree_insert_t insert_fix_t) (set! tree_insert_idx insert_fix_z) __res))) (throw (ex-info "return" {:v tree_insert_t}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var tree_insert_t) (constantly tree_insert_t))))))

(defn inorder [inorder_t inorder_x inorder_acc_p]
  (binding [inorder_acc inorder_acc_p] (try (do (when (= inorder_x main_NEG_ONE) (throw (ex-info "return" {:v inorder_acc}))) (set! inorder_acc (let [__res (inorder inorder_t (get (get (:nodes inorder_t) inorder_x) main_LEFT) inorder_acc)] (do (set! inorder_acc inorder_acc) __res))) (set! inorder_acc (conj inorder_acc (get (get (:nodes inorder_t) inorder_x) main_LABEL))) (set! inorder_acc (let [__res (inorder inorder_t (get (get (:nodes inorder_t) inorder_x) main_RIGHT) inorder_acc)] (do (set! inorder_acc inorder_acc) __res))) (throw (ex-info "return" {:v inorder_acc}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var inorder_acc) (constantly inorder_acc))))))

(defn main []
  (binding [main_i nil main_res nil main_t nil main_values nil] (do (set! main_t (make_tree)) (set! main_values [10 20 30 15 25 5 1]) (set! main_i 0) (while (< main_i (count main_values)) (do (set! main_t (let [__res (tree_insert main_t (nth main_values main_i))] (do (set! main_t tree_insert_t) __res))) (set! main_i (+ main_i 1)))) (set! main_res []) (set! main_res (let [__res (inorder main_t (:root main_t) main_res)] (do (set! main_res inorder_acc) __res))) (println (mochi_str main_res)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_LABEL) (constantly 0))
      (alter-var-root (var main_COLOR) (constantly 1))
      (alter-var-root (var main_PARENT) (constantly 2))
      (alter-var-root (var main_LEFT) (constantly 3))
      (alter-var-root (var main_RIGHT) (constantly 4))
      (alter-var-root (var main_NEG_ONE) (constantly (- 1)))
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
