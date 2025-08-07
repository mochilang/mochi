(ns main (:refer-clojure :exclude [create_board move_ant langtons_ant]))

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

(declare create_board move_ant langtons_ant)

(def ^:dynamic create_board_board nil)

(def ^:dynamic create_board_i nil)

(def ^:dynamic create_board_j nil)

(def ^:dynamic create_board_row nil)

(def ^:dynamic langtons_ant_board nil)

(def ^:dynamic langtons_ant_dir nil)

(def ^:dynamic langtons_ant_s nil)

(def ^:dynamic langtons_ant_state nil)

(def ^:dynamic langtons_ant_x nil)

(def ^:dynamic langtons_ant_y nil)

(def ^:dynamic move_ant_board nil)

(def ^:dynamic move_ant_direction nil)

(def ^:dynamic move_ant_old_x nil)

(def ^:dynamic move_ant_old_y nil)

(def ^:dynamic move_ant_x nil)

(def ^:dynamic move_ant_y nil)

(defn create_board [create_board_width create_board_height]
  (binding [create_board_board nil create_board_i nil create_board_j nil create_board_row nil] (try (do (set! create_board_board []) (set! create_board_i 0) (while (< create_board_i create_board_height) (do (set! create_board_row []) (set! create_board_j 0) (while (< create_board_j create_board_width) (do (set! create_board_row (conj create_board_row true)) (set! create_board_j (+ create_board_j 1)))) (set! create_board_board (conj create_board_board create_board_row)) (set! create_board_i (+ create_board_i 1)))) (throw (ex-info "return" {:v create_board_board}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn move_ant [move_ant_board_p move_ant_x_p move_ant_y_p move_ant_direction_p]
  (binding [move_ant_board nil move_ant_direction nil move_ant_old_x nil move_ant_old_y nil move_ant_x nil move_ant_y nil] (try (do (set! move_ant_board move_ant_board_p) (set! move_ant_x move_ant_x_p) (set! move_ant_y move_ant_y_p) (set! move_ant_direction move_ant_direction_p) (if (nth (nth move_ant_board move_ant_x) move_ant_y) (set! move_ant_direction (mod (+ move_ant_direction 1) 4)) (set! move_ant_direction (mod (+ move_ant_direction 3) 4))) (set! move_ant_old_x move_ant_x) (set! move_ant_old_y move_ant_y) (if (= move_ant_direction 0) (set! move_ant_x (- move_ant_x 1)) (if (= move_ant_direction 1) (set! move_ant_y (+ move_ant_y 1)) (if (= move_ant_direction 2) (set! move_ant_x (+ move_ant_x 1)) (set! move_ant_y (- move_ant_y 1))))) (set! move_ant_board (assoc-in move_ant_board [move_ant_old_x move_ant_old_y] (not (nth (nth move_ant_board move_ant_old_x) move_ant_old_y)))) (throw (ex-info "return" {:v [move_ant_x move_ant_y move_ant_direction]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn langtons_ant [langtons_ant_width langtons_ant_height langtons_ant_steps]
  (binding [langtons_ant_board nil langtons_ant_dir nil langtons_ant_s nil langtons_ant_state nil langtons_ant_x nil langtons_ant_y nil] (try (do (set! langtons_ant_board (create_board langtons_ant_width langtons_ant_height)) (set! langtons_ant_x (quot langtons_ant_width 2)) (set! langtons_ant_y (quot langtons_ant_height 2)) (set! langtons_ant_dir 3) (set! langtons_ant_s 0) (while (< langtons_ant_s langtons_ant_steps) (do (set! langtons_ant_state (move_ant langtons_ant_board langtons_ant_x langtons_ant_y langtons_ant_dir)) (set! langtons_ant_x (nth langtons_ant_state 0)) (set! langtons_ant_y (nth langtons_ant_state 1)) (set! langtons_ant_dir (nth langtons_ant_state 2)) (set! langtons_ant_s (+ langtons_ant_s 1)))) (throw (ex-info "return" {:v langtons_ant_board}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]

      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
