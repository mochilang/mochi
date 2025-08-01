(ns main (:refer-clojure :exclude [parseIntBase intToBase subset]))

(require 'clojure.set)

(defrecord TestCases [base begin end kaprekar])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare intToBase_d intToBase_digits intToBase_out intToBase_v main_found main_i main_idx main_k main_s main_sx main_tc main_testCases main_valid parseIntBase_digits parseIntBase_i parseIntBase_j parseIntBase_n parseIntBase_v subset_b subset_e subset_k subset_ks subset_mod subset_out subset_r1 subset_r2)

(declare parseIntBase intToBase subset)

(defn parseIntBase [parseIntBase_s parseIntBase_base]
  (try (do (def parseIntBase_digits "0123456789abcdefghijklmnopqrstuvwxyz") (def parseIntBase_n 0) (def parseIntBase_i 0) (while (< parseIntBase_i (count parseIntBase_s)) (do (def parseIntBase_j 0) (def parseIntBase_v 0) (loop [while_flag_1 true] (when (and while_flag_1 (< parseIntBase_j (count parseIntBase_digits))) (cond (= (subs parseIntBase_digits parseIntBase_j (+ parseIntBase_j 1)) (subs parseIntBase_s parseIntBase_i (+ parseIntBase_i 1))) (do (def parseIntBase_v parseIntBase_j) (recur false)) :else (do (def parseIntBase_j (+ parseIntBase_j 1)) (recur while_flag_1))))) (def parseIntBase_n (+ (* parseIntBase_n parseIntBase_base) parseIntBase_v)) (def parseIntBase_i (+ parseIntBase_i 1)))) (throw (ex-info "return" {:v parseIntBase_n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn intToBase [intToBase_n intToBase_base]
  (try (do (def intToBase_digits "0123456789abcdefghijklmnopqrstuvwxyz") (when (= intToBase_n 0) (throw (ex-info "return" {:v "0"}))) (def intToBase_out "") (def intToBase_v intToBase_n) (while (> intToBase_v 0) (do (def intToBase_d (mod intToBase_v intToBase_base)) (def intToBase_out (+ (subvec intToBase_digits intToBase_d (+ intToBase_d 1)) intToBase_out)) (def intToBase_v (/ intToBase_v intToBase_base)))) (throw (ex-info "return" {:v intToBase_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn subset [subset_base subset_begin subset_end]
  (try (do (def subset_b (parseIntBase subset_begin subset_base)) (def subset_e (parseIntBase subset_end subset_base)) (def subset_out []) (def subset_k subset_b) (while (<= subset_k subset_e) (do (def subset_ks (intToBase subset_k subset_base)) (def subset_mod (- subset_base 1)) (def subset_r1 (mod (parseIntBase subset_ks subset_base) subset_mod)) (def subset_r2 (mod (* (parseIntBase subset_ks subset_base) (parseIntBase subset_ks subset_base)) subset_mod)) (when (= subset_r1 subset_r2) (def subset_out (conj subset_out subset_ks))) (def subset_k (+ subset_k 1)))) (throw (ex-info "return" {:v subset_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def main_testCases [{"base" 10 "begin" "1" "end" "100" "kaprekar" ["1" "9" "45" "55" "99"]} {"base" 17 "begin" "10" "end" "gg" "kaprekar" ["3d" "d4" "gg"]}])

(def main_idx 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_idx (count main_testCases)) (do (def main_tc (nth main_testCases main_idx)) (println (str (str (str (str (str (str "\nTest case base = " (str (get main_tc "base"))) ", begin = ") (get main_tc "begin")) ", end = ") (get main_tc "end")) ":")) (def main_s (subset (get main_tc "base") (get main_tc "begin") (get main_tc "end"))) (println (str "Subset:  " (str main_s))) (println (str "Kaprekar:" (str (get main_tc "kaprekar")))) (def main_sx 0) (def main_valid true) (def main_i 0) (loop [while_flag_2 true] (when (and while_flag_2 (< main_i (count (get main_tc "kaprekar")))) (do (def main_k (nth (get main_tc "kaprekar") main_i)) (def main_found false) (loop [while_flag_3 true] (when (and while_flag_3 (< main_sx (count main_s))) (cond (= (nth main_s main_sx) main_k) (do (def main_found true) (def main_sx (+ main_sx 1)) (recur false)) :else (do (def main_sx (+ main_sx 1)) (recur while_flag_3))))) (cond (not main_found) (do (println (str (str "Fail:" main_k) " not in subset")) (def main_valid false) (recur false)) :else (do (def main_i (+ main_i 1)) (recur while_flag_2)))))) (when main_valid (println "Valid subset.")) (def main_idx (+ main_idx 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
