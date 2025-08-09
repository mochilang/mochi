(ns main (:refer-clojure :exclude [parse_project_name]))

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

(declare parse_project_name)

(def ^:dynamic parse_project_name_i nil)

(def ^:dynamic parse_project_name_n nil)

(def ^:dynamic parse_project_name_name nil)

(defn parse_project_name [parse_project_name_toml]
  (binding [parse_project_name_i nil parse_project_name_n nil parse_project_name_name nil] (try (do (set! parse_project_name_i 0) (set! parse_project_name_name "") (set! parse_project_name_n (count parse_project_name_toml)) (while (< (+ parse_project_name_i 4) parse_project_name_n) (do (when (and (and (and (= (subs parse_project_name_toml parse_project_name_i (+ parse_project_name_i 1)) "n") (= (subs parse_project_name_toml (+ parse_project_name_i 1) (+ (+ parse_project_name_i 1) 1)) "a")) (= (subs parse_project_name_toml (+ parse_project_name_i 2) (+ (+ parse_project_name_i 2) 1)) "m")) (= (subs parse_project_name_toml (+ parse_project_name_i 3) (+ (+ parse_project_name_i 3) 1)) "e")) (do (set! parse_project_name_i (+ parse_project_name_i 4)) (while (and (< parse_project_name_i parse_project_name_n) (not= (subs parse_project_name_toml parse_project_name_i (+ parse_project_name_i 1)) "\"")) (set! parse_project_name_i (+ parse_project_name_i 1))) (set! parse_project_name_i (+ parse_project_name_i 1)) (while (and (< parse_project_name_i parse_project_name_n) (not= (subs parse_project_name_toml parse_project_name_i (+ parse_project_name_i 1)) "\"")) (do (set! parse_project_name_name (str parse_project_name_name (subs parse_project_name_toml parse_project_name_i (+ parse_project_name_i 1)))) (set! parse_project_name_i (+ parse_project_name_i 1)))) (throw (ex-info "return" {:v parse_project_name_name})))) (set! parse_project_name_i (+ parse_project_name_i 1)))) (throw (ex-info "return" {:v parse_project_name_name}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_pyproject "[project]\nname = \"thealgorithms-python\"")

(def ^:dynamic main_project (parse_project_name main_pyproject))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println main_project)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
