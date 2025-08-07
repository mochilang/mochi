(ns main (:refer-clojure :exclude [rand rand_range shuffle create_board create_prey create_predator empty_cell add_entity setup inside find_prey step_world count_entities]))

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

(declare rand rand_range shuffle create_board create_prey create_predator empty_cell add_entity setup inside find_prey step_world count_entities)

(def ^:dynamic add_entity_c nil)

(def ^:dynamic add_entity_r nil)

(def ^:dynamic count_entities_cnt nil)

(def ^:dynamic count_entities_i nil)

(def ^:dynamic create_board_board nil)

(def ^:dynamic create_board_c nil)

(def ^:dynamic create_board_r nil)

(def ^:dynamic create_board_row nil)

(def ^:dynamic find_prey_e nil)

(def ^:dynamic find_prey_i nil)

(def ^:dynamic main_t nil)

(def ^:dynamic setup_i nil)

(def ^:dynamic shuffle_i nil)

(def ^:dynamic shuffle_j nil)

(def ^:dynamic shuffle_list_int nil)

(def ^:dynamic shuffle_tmp nil)

(def ^:dynamic step_world_alive nil)

(def ^:dynamic step_world_ate nil)

(def ^:dynamic step_world_col nil)

(def ^:dynamic step_world_d nil)

(def ^:dynamic step_world_dirs nil)

(def ^:dynamic step_world_e nil)

(def ^:dynamic step_world_e2 nil)

(def ^:dynamic step_world_energy nil)

(def ^:dynamic step_world_i nil)

(def ^:dynamic step_world_j nil)

(def ^:dynamic step_world_k nil)

(def ^:dynamic step_world_moved nil)

(def ^:dynamic step_world_nc nil)

(def ^:dynamic step_world_nr nil)

(def ^:dynamic step_world_old_c nil)

(def ^:dynamic step_world_old_r nil)

(def ^:dynamic step_world_prey_index nil)

(def ^:dynamic step_world_repro nil)

(def ^:dynamic step_world_row nil)

(def ^:dynamic step_world_typ nil)

(def ^:dynamic main_WIDTH 10)

(def ^:dynamic main_HEIGHT 10)

(def ^:dynamic main_PREY_INITIAL_COUNT 20)

(def ^:dynamic main_PREY_REPRODUCTION_TIME 5)

(def ^:dynamic main_PREDATOR_INITIAL_COUNT 5)

(def ^:dynamic main_PREDATOR_REPRODUCTION_TIME 20)

(def ^:dynamic main_PREDATOR_INITIAL_ENERGY 15)

(def ^:dynamic main_PREDATOR_FOOD_VALUE 5)

(def ^:dynamic main_TYPE_PREY 0)

(def ^:dynamic main_TYPE_PREDATOR 1)

(def ^:dynamic main_seed 123456789)

(defn rand []
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* main_seed 1103515245) 12345) 2147483648))) (throw (ex-info "return" {:v main_seed}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rand_range [rand_range_max]
  (try (throw (ex-info "return" {:v (mod (rand) rand_range_max)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn shuffle [shuffle_list_int_p]
  (binding [shuffle_i nil shuffle_j nil shuffle_list_int nil shuffle_tmp nil] (try (do (set! shuffle_list_int shuffle_list_int_p) (set! shuffle_i (- (count shuffle_list_int) 1)) (while (> shuffle_i 0) (do (set! shuffle_j (rand_range (+ shuffle_i 1))) (set! shuffle_tmp (nth shuffle_list_int shuffle_i)) (set! shuffle_list_int (assoc shuffle_list_int shuffle_i (nth shuffle_list_int shuffle_j))) (set! shuffle_list_int (assoc shuffle_list_int shuffle_j shuffle_tmp)) (set! shuffle_i (- shuffle_i 1)))) (throw (ex-info "return" {:v shuffle_list_int}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn create_board []
  (binding [create_board_board nil create_board_c nil create_board_r nil create_board_row nil] (try (do (set! create_board_board []) (set! create_board_r 0) (while (< create_board_r main_HEIGHT) (do (set! create_board_row []) (set! create_board_c 0) (while (< create_board_c main_WIDTH) (do (set! create_board_row (conj create_board_row 0)) (set! create_board_c (+ create_board_c 1)))) (set! create_board_board (conj create_board_board create_board_row)) (set! create_board_r (+ create_board_r 1)))) (throw (ex-info "return" {:v create_board_board}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn create_prey [create_prey_r create_prey_c]
  (try (throw (ex-info "return" {:v [main_TYPE_PREY create_prey_r create_prey_c main_PREY_REPRODUCTION_TIME 0 1]})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn create_predator [create_predator_r create_predator_c]
  (try (throw (ex-info "return" {:v [main_TYPE_PREDATOR create_predator_r create_predator_c main_PREDATOR_REPRODUCTION_TIME main_PREDATOR_INITIAL_ENERGY 1]})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_board (create_board))

(def ^:dynamic main_entities [])

(defn empty_cell [empty_cell_r empty_cell_c]
  (try (throw (ex-info "return" {:v (= (nth (nth main_board empty_cell_r) empty_cell_c) 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn add_entity [add_entity_typ]
  (binding [add_entity_c nil add_entity_r nil] (try (while true (do (set! add_entity_r (rand_range main_HEIGHT)) (set! add_entity_c (rand_range main_WIDTH)) (when (empty_cell add_entity_r add_entity_c) (do (if (= add_entity_typ main_TYPE_PREY) (do (alter-var-root (var main_board) (fn [_] (assoc-in main_board [add_entity_r add_entity_c] 1))) (alter-var-root (var main_entities) (fn [_] (conj main_entities (create_prey add_entity_r add_entity_c))))) (do (alter-var-root (var main_board) (fn [_] (assoc-in main_board [add_entity_r add_entity_c] 2))) (alter-var-root (var main_entities) (fn [_] (conj main_entities (create_predator add_entity_r add_entity_c)))))) (throw (ex-info "return" {:v nil})))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn setup []
  (binding [setup_i nil] (do (set! setup_i 0) (while (< setup_i main_PREY_INITIAL_COUNT) (do (add_entity main_TYPE_PREY) (set! setup_i (+ setup_i 1)))) (set! setup_i 0) (while (< setup_i main_PREDATOR_INITIAL_COUNT) (do (add_entity main_TYPE_PREDATOR) (set! setup_i (+ setup_i 1)))))))

(def ^:dynamic main_dr [(- 1) 0 1 0])

(def ^:dynamic main_dc [0 1 0 (- 1)])

(defn inside [inside_r inside_c]
  (try (throw (ex-info "return" {:v (and (and (and (>= inside_r 0) (< inside_r main_HEIGHT)) (>= inside_c 0)) (< inside_c main_WIDTH))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn find_prey [find_prey_r find_prey_c]
  (binding [find_prey_e nil find_prey_i nil] (try (do (set! find_prey_i 0) (while (< find_prey_i (count main_entities)) (do (set! find_prey_e (nth main_entities find_prey_i)) (when (and (and (and (= (nth find_prey_e 5) 1) (= (nth find_prey_e 0) main_TYPE_PREY)) (= (nth find_prey_e 1) find_prey_r)) (= (nth find_prey_e 2) find_prey_c)) (throw (ex-info "return" {:v find_prey_i}))) (set! find_prey_i (+ find_prey_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn step_world []
  (binding [step_world_alive nil step_world_ate nil step_world_col nil step_world_d nil step_world_dirs nil step_world_e nil step_world_e2 nil step_world_energy nil step_world_i nil step_world_j nil step_world_k nil step_world_moved nil step_world_nc nil step_world_nr nil step_world_old_c nil step_world_old_r nil step_world_prey_index nil step_world_repro nil step_world_row nil step_world_typ nil] (do (set! step_world_i 0) (loop [while_flag_1 true] (when (and while_flag_1 (< step_world_i (count main_entities))) (do (set! step_world_e (nth main_entities step_world_i)) (cond (= (nth step_world_e 5) 0) (do (set! step_world_i (+ step_world_i 1)) (recur true)) :else (do (set! step_world_typ (nth step_world_e 0)) (set! step_world_row (nth step_world_e 1)) (set! step_world_col (nth step_world_e 2)) (set! step_world_repro (nth step_world_e 3)) (set! step_world_energy (nth step_world_e 4)) (set! step_world_dirs [0 1 2 3]) (set! step_world_dirs (shuffle step_world_dirs)) (set! step_world_moved false) (set! step_world_old_r step_world_row) (set! step_world_old_c step_world_col) (if (= step_world_typ main_TYPE_PREDATOR) (do (set! step_world_j 0) (set! step_world_ate false) (loop [while_flag_2 true] (when (and while_flag_2 (< step_world_j 4)) (do (set! step_world_d (nth step_world_dirs step_world_j)) (set! step_world_nr (+ step_world_row (nth main_dr step_world_d))) (set! step_world_nc (+ step_world_col (nth main_dc step_world_d))) (cond (and (inside step_world_nr step_world_nc) (= (nth (nth main_board step_world_nr) step_world_nc) 1)) (do (set! step_world_prey_index (find_prey step_world_nr step_world_nc)) (when (>= step_world_prey_index 0) (alter-var-root (var main_entities) (fn [_] (assoc-in main_entities [step_world_prey_index 5] 0)))) (alter-var-root (var main_board) (fn [_] (assoc-in main_board [step_world_nr step_world_nc] 2))) (alter-var-root (var main_board) (fn [_] (assoc-in main_board [step_world_row step_world_col] 0))) (set! step_world_e (assoc step_world_e 1 step_world_nr)) (set! step_world_e (assoc step_world_e 2 step_world_nc)) (set! step_world_e (assoc step_world_e 4 (- (+ step_world_energy main_PREDATOR_FOOD_VALUE) 1))) (set! step_world_moved true) (set! step_world_ate true) (recur false)) :else (do (set! step_world_j (+ step_world_j 1)) (recur while_flag_2)))))) (when (not step_world_ate) (do (set! step_world_j 0) (loop [while_flag_3 true] (when (and while_flag_3 (< step_world_j 4)) (do (set! step_world_d (nth step_world_dirs step_world_j)) (set! step_world_nr (+ step_world_row (nth main_dr step_world_d))) (set! step_world_nc (+ step_world_col (nth main_dc step_world_d))) (cond (and (inside step_world_nr step_world_nc) (= (nth (nth main_board step_world_nr) step_world_nc) 0)) (do (alter-var-root (var main_board) (fn [_] (assoc-in main_board [step_world_nr step_world_nc] 2))) (alter-var-root (var main_board) (fn [_] (assoc-in main_board [step_world_row step_world_col] 0))) (set! step_world_e (assoc step_world_e 1 step_world_nr)) (set! step_world_e (assoc step_world_e 2 step_world_nc)) (set! step_world_moved true) (recur false)) :else (do (set! step_world_j (+ step_world_j 1)) (recur while_flag_3)))))) (set! step_world_e (assoc step_world_e 4 (- step_world_energy 1))))) (when (<= (nth step_world_e 4) 0) (do (set! step_world_e (assoc step_world_e 5 0)) (alter-var-root (var main_board) (fn [_] (assoc-in main_board [(nth step_world_e 1) (nth step_world_e 2)] 0)))))) (do (set! step_world_j 0) (loop [while_flag_4 true] (when (and while_flag_4 (< step_world_j 4)) (do (set! step_world_d (nth step_world_dirs step_world_j)) (set! step_world_nr (+ step_world_row (nth main_dr step_world_d))) (set! step_world_nc (+ step_world_col (nth main_dc step_world_d))) (cond (and (inside step_world_nr step_world_nc) (= (nth (nth main_board step_world_nr) step_world_nc) 0)) (do (alter-var-root (var main_board) (fn [_] (assoc-in main_board [step_world_nr step_world_nc] 1))) (alter-var-root (var main_board) (fn [_] (assoc-in main_board [step_world_row step_world_col] 0))) (set! step_world_e (assoc step_world_e 1 step_world_nr)) (set! step_world_e (assoc step_world_e 2 step_world_nc)) (set! step_world_moved true) (recur false)) :else (do (set! step_world_j (+ step_world_j 1)) (recur while_flag_4)))))))) (when (= (nth step_world_e 5) 1) (if (and step_world_moved (<= step_world_repro 0)) (if (= step_world_typ main_TYPE_PREY) (do (alter-var-root (var main_board) (fn [_] (assoc-in main_board [step_world_old_r step_world_old_c] 1))) (alter-var-root (var main_entities) (fn [_] (conj main_entities (create_prey step_world_old_r step_world_old_c)))) (set! step_world_e (assoc step_world_e 3 main_PREY_REPRODUCTION_TIME))) (do (alter-var-root (var main_board) (fn [_] (assoc-in main_board [step_world_old_r step_world_old_c] 2))) (alter-var-root (var main_entities) (fn [_] (conj main_entities (create_predator step_world_old_r step_world_old_c)))) (set! step_world_e (assoc step_world_e 3 main_PREDATOR_REPRODUCTION_TIME)))) (set! step_world_e (assoc step_world_e 3 (- step_world_repro 1))))) (set! step_world_i (+ step_world_i 1)) (recur while_flag_1)))))) (set! step_world_alive []) (set! step_world_k 0) (while (< step_world_k (count main_entities)) (do (set! step_world_e2 (nth main_entities step_world_k)) (when (= (nth step_world_e2 5) 1) (set! step_world_alive (conj step_world_alive step_world_e2))) (set! step_world_k (+ step_world_k 1)))) (alter-var-root (var main_entities) (fn [_] step_world_alive)))))

(defn count_entities [count_entities_typ]
  (binding [count_entities_cnt nil count_entities_i nil] (try (do (set! count_entities_cnt 0) (set! count_entities_i 0) (while (< count_entities_i (count main_entities)) (do (when (and (= (nth (nth main_entities count_entities_i) 0) count_entities_typ) (= (nth (nth main_entities count_entities_i) 5) 1)) (set! count_entities_cnt (+ count_entities_cnt 1))) (set! count_entities_i (+ count_entities_i 1)))) (throw (ex-info "return" {:v count_entities_cnt}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_t 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (setup)
      (while (< main_t 10) (do (step_world) (def main_t (+ main_t 1))))
      (println (str "Prey: " (str (count_entities main_TYPE_PREY))))
      (println (str "Predators: " (str (count_entities main_TYPE_PREDATOR))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
