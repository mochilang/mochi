(ns main (:refer-clojure :exclude [trimSpace isUpper padLeft snakeToCamel camelToSnake main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare trimSpace isUpper padLeft snakeToCamel camelToSnake main)

(declare camelToSnake_c camelToSnake_ch camelToSnake_end camelToSnake_i camelToSnake_j camelToSnake_lastUnd camelToSnake_out camelToSnake_prevUnd camelToSnake_res camelToSnake_start main_samples padLeft_n padLeft_res snakeToCamel_ch snakeToCamel_i snakeToCamel_out snakeToCamel_up trimSpace_end trimSpace_start)

(defn trimSpace [trimSpace_s]
  (try (do (def trimSpace_start 0) (while (and (< trimSpace_start (count trimSpace_s)) (= (subs trimSpace_s trimSpace_start (+ trimSpace_start 1)) " ")) (def trimSpace_start (+ trimSpace_start 1))) (def trimSpace_end (count trimSpace_s)) (while (and (> trimSpace_end trimSpace_start) (= (subs trimSpace_s (- trimSpace_end 1) trimSpace_end) " ")) (def trimSpace_end (- trimSpace_end 1))) (throw (ex-info "return" {:v (subs trimSpace_s trimSpace_start trimSpace_end)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn isUpper [isUpper_ch]
  (try (throw (ex-info "return" {:v (and (>= (compare isUpper_ch "A") 0) (<= (compare isUpper_ch "Z") 0))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn padLeft [padLeft_s padLeft_w]
  (try (do (def padLeft_res "") (def padLeft_n (- padLeft_w (count padLeft_s))) (while (> padLeft_n 0) (do (def padLeft_res (str padLeft_res " ")) (def padLeft_n (- padLeft_n 1)))) (throw (ex-info "return" {:v (str padLeft_res padLeft_s)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn snakeToCamel [snakeToCamel_s_p]
  (try (do (def snakeToCamel_s snakeToCamel_s_p) (def snakeToCamel_s (trimSpace snakeToCamel_s)) (def snakeToCamel_out "") (def snakeToCamel_up false) (def snakeToCamel_i 0) (loop [while_flag_1 true] (when (and while_flag_1 (< snakeToCamel_i (count snakeToCamel_s))) (do (def snakeToCamel_ch (subs snakeToCamel_s snakeToCamel_i (+ snakeToCamel_i 1))) (cond (or (or (or (= snakeToCamel_ch "_") (= snakeToCamel_ch "-")) (= snakeToCamel_ch " ")) (= snakeToCamel_ch ".")) (do (def snakeToCamel_up true) (def snakeToCamel_i (+ snakeToCamel_i 1)) (recur true)) (= snakeToCamel_i 0) (do (def snakeToCamel_out (str snakeToCamel_out (clojure.string/lower-case snakeToCamel_ch))) (def snakeToCamel_up false) (def snakeToCamel_i (+ snakeToCamel_i 1)) (recur true)) :else (do (if snakeToCamel_up (do (def snakeToCamel_out (str snakeToCamel_out (clojure.string/upper-case snakeToCamel_ch))) (def snakeToCamel_up false)) (def snakeToCamel_out (str snakeToCamel_out snakeToCamel_ch))) (def snakeToCamel_i (+ snakeToCamel_i 1)) (recur while_flag_1)))))) (throw (ex-info "return" {:v snakeToCamel_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn camelToSnake [camelToSnake_s_p]
  (try (do (def camelToSnake_s camelToSnake_s_p) (def camelToSnake_s (trimSpace camelToSnake_s)) (def camelToSnake_out "") (def camelToSnake_prevUnd false) (def camelToSnake_i 0) (loop [while_flag_2 true] (when (and while_flag_2 (< camelToSnake_i (count camelToSnake_s))) (do (def camelToSnake_ch (subs camelToSnake_s camelToSnake_i (+ camelToSnake_i 1))) (cond (or (or (= camelToSnake_ch " ") (= camelToSnake_ch "-")) (= camelToSnake_ch ".")) (do (when (and (not camelToSnake_prevUnd) (> (count camelToSnake_out) 0)) (do (def camelToSnake_out (str camelToSnake_out "_")) (def camelToSnake_prevUnd true))) (def camelToSnake_i (+ camelToSnake_i 1)) (recur true)) (= camelToSnake_ch "_") (do (when (and (not camelToSnake_prevUnd) (> (count camelToSnake_out) 0)) (do (def camelToSnake_out (str camelToSnake_out "_")) (def camelToSnake_prevUnd true))) (def camelToSnake_i (+ camelToSnake_i 1)) (recur true)) :else (do (if (isUpper camelToSnake_ch) (do (when (and (> camelToSnake_i 0) (not camelToSnake_prevUnd)) (def camelToSnake_out (str camelToSnake_out "_"))) (def camelToSnake_out (str camelToSnake_out (clojure.string/lower-case camelToSnake_ch))) (def camelToSnake_prevUnd false)) (do (def camelToSnake_out (str camelToSnake_out (clojure.string/lower-case camelToSnake_ch))) (def camelToSnake_prevUnd false))) (def camelToSnake_i (+ camelToSnake_i 1)) (recur while_flag_2)))))) (def camelToSnake_start 0) (while (and (< camelToSnake_start (count camelToSnake_out)) (= (subs camelToSnake_out camelToSnake_start (+ camelToSnake_start 1)) "_")) (def camelToSnake_start (+ camelToSnake_start 1))) (def camelToSnake_end (count camelToSnake_out)) (while (and (> camelToSnake_end camelToSnake_start) (= (subs camelToSnake_out (- camelToSnake_end 1) camelToSnake_end) "_")) (def camelToSnake_end (- camelToSnake_end 1))) (def camelToSnake_out (subs camelToSnake_out camelToSnake_start camelToSnake_end)) (def camelToSnake_res "") (def camelToSnake_j 0) (def camelToSnake_lastUnd false) (while (< camelToSnake_j (count camelToSnake_out)) (do (def camelToSnake_c (subs camelToSnake_out camelToSnake_j (+ camelToSnake_j 1))) (if (= camelToSnake_c "_") (do (when (not camelToSnake_lastUnd) (def camelToSnake_res (str camelToSnake_res camelToSnake_c))) (def camelToSnake_lastUnd true)) (do (def camelToSnake_res (str camelToSnake_res camelToSnake_c)) (def camelToSnake_lastUnd false))) (def camelToSnake_j (+ camelToSnake_j 1)))) (throw (ex-info "return" {:v camelToSnake_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def main_samples ["snakeCase" "snake_case" "snake-case" "snake case" "snake CASE" "snake.case" "variable_10_case" "variable10Case" "ɛrgo rE tHis" "hurry-up-joe!" "c://my-docs/happy_Flag-Day/12.doc" " spaces "]) (println "=== To snake_case ===") (doseq [s main_samples] (println (str (str (padLeft s 34) " => ") (camelToSnake s)))) (println "") (println "=== To camelCase ===") (doseq [s main_samples] (println (str (str (padLeft s 34) " => ") (snakeToCamel s))))))

(defn -main []
  (main))

(-main)
