(ns main (:refer-clojure :exclude [josephus_recursive find_winner remove_at josephus_iterative]))

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

(declare josephus_recursive find_winner remove_at josephus_iterative)

(def ^:dynamic josephus_iterative_circle nil)

(def ^:dynamic josephus_iterative_current nil)

(def ^:dynamic josephus_iterative_i nil)

(def ^:dynamic remove_at_i nil)

(def ^:dynamic remove_at_res nil)

(defn josephus_recursive [josephus_recursive_num_people josephus_recursive_step_size]
  (try (do (when (or (<= josephus_recursive_num_people 0) (<= josephus_recursive_step_size 0)) (throw (Exception. "num_people or step_size is not a positive integer."))) (if (= josephus_recursive_num_people 1) 0 (mod (+ (josephus_recursive (- josephus_recursive_num_people 1) josephus_recursive_step_size) josephus_recursive_step_size) josephus_recursive_num_people))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn find_winner [find_winner_num_people find_winner_step_size]
  (try (throw (ex-info "return" {:v (+ (josephus_recursive find_winner_num_people find_winner_step_size) 1)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn remove_at [remove_at_xs remove_at_idx]
  (binding [remove_at_i nil remove_at_res nil] (try (do (set! remove_at_res []) (set! remove_at_i 0) (while (< remove_at_i (count remove_at_xs)) (do (when (not= remove_at_i remove_at_idx) (set! remove_at_res (conj remove_at_res (nth remove_at_xs remove_at_i)))) (set! remove_at_i (+ remove_at_i 1)))) (throw (ex-info "return" {:v remove_at_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn josephus_iterative [josephus_iterative_num_people josephus_iterative_step_size]
  (binding [josephus_iterative_circle nil josephus_iterative_current nil josephus_iterative_i nil] (try (do (when (or (<= josephus_iterative_num_people 0) (<= josephus_iterative_step_size 0)) (throw (Exception. "num_people or step_size is not a positive integer."))) (set! josephus_iterative_circle []) (set! josephus_iterative_i 1) (while (<= josephus_iterative_i josephus_iterative_num_people) (do (set! josephus_iterative_circle (conj josephus_iterative_circle josephus_iterative_i)) (set! josephus_iterative_i (+ josephus_iterative_i 1)))) (set! josephus_iterative_current 0) (while (> (count josephus_iterative_circle) 1) (do (set! josephus_iterative_current (mod (- (+ josephus_iterative_current josephus_iterative_step_size) 1) (count josephus_iterative_circle))) (set! josephus_iterative_circle (remove_at josephus_iterative_circle josephus_iterative_current)))) (throw (ex-info "return" {:v (nth josephus_iterative_circle 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_r (josephus_recursive 7 3))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str main_r))
      (println (str (find_winner 7 3)))
      (println (str (josephus_iterative 7 3)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
