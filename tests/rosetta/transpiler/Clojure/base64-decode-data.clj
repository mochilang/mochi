(ns main (:refer-clojure :exclude [indexOf parseIntStr ord chr toBinary binToInt base64Encode base64Decode]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare indexOf parseIntStr ord chr toBinary binToInt base64Encode base64Decode)

(defn indexOf [s ch]
  (try (do (def i 0) (while (< i (count s)) (do (when (= (nth s i) ch) (throw (ex-info "return" {:v i}))) (def i (+ i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn parseIntStr [str]
  (try (do (def i 0) (def neg false) (when (and (> (count str) 0) (= (nth str 0) "-")) (do (def neg true) (def i 1))) (def n 0) (def digits {"0" 0 "1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9}) (while (< i (count str)) (do (def n (+ (* n 10) (nth digits (nth str i)))) (def i (+ i 1)))) (when neg (def n (- n))) (throw (ex-info "return" {:v n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn ord [ch]
  (try (do (def upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (def lower "abcdefghijklmnopqrstuvwxyz") (def idx (indexOf upper ch)) (when (>= idx 0) (throw (ex-info "return" {:v (+ 65 idx)}))) (def idx (indexOf lower ch)) (when (>= idx 0) (throw (ex-info "return" {:v (+ 97 idx)}))) (when (and (>= (compare ch "0") 0) (<= (compare ch "9") 0)) (throw (ex-info "return" {:v (+ 48 (parseIntStr ch))}))) (when (= ch "+") (throw (ex-info "return" {:v 43}))) (when (= ch "/") (throw (ex-info "return" {:v 47}))) (when (= ch " ") (throw (ex-info "return" {:v 32}))) (if (= ch "=") 61 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn chr [n]
  (try (do (def upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (def lower "abcdefghijklmnopqrstuvwxyz") (when (and (>= n 65) (< n 91)) (throw (ex-info "return" {:v (subs upper (- n 65) (- n 64))}))) (when (and (>= n 97) (< n 123)) (throw (ex-info "return" {:v (subs lower (- n 97) (- n 96))}))) (when (and (>= n 48) (< n 58)) (do (def digits "0123456789") (throw (ex-info "return" {:v (subs digits (- n 48) (- n 47))})))) (when (= n 43) (throw (ex-info "return" {:v "+"}))) (when (= n 47) (throw (ex-info "return" {:v "/"}))) (when (= n 32) (throw (ex-info "return" {:v " "}))) (if (= n 61) "=" "?")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn toBinary [n bits]
  (try (do (def b "") (def val n) (def i 0) (while (< i bits) (do (def b (str (str (mod val 2)) b)) (def val (int (/ val 2))) (def i (+ i 1)))) (throw (ex-info "return" {:v b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn binToInt [bits]
  (try (do (def n 0) (def i 0) (while (< i (count bits)) (do (def n (+ (* n 2) (parseIntStr (subs bits i (+ i 1))))) (def i (+ i 1)))) (throw (ex-info "return" {:v n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn base64Encode [text]
  (try (do (def alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/") (def bin "") (doseq [ch text] (def bin (str bin (toBinary (ord ch) 8)))) (while (not= (mod (count bin) 6) 0) (def bin (str bin "0"))) (def out "") (def i 0) (while (< i (count bin)) (do (def chunk (subs bin i (+ i 6))) (def val (binToInt chunk)) (def out (str out (subs alphabet val (+ val 1)))) (def i (+ i 6)))) (def pad (mod (- 3 (mod (count text) 3)) 3)) (when (= pad 1) (def out (str (subs out 0 (- (count out) 1)) "="))) (when (= pad 2) (def out (str (subs out 0 (- (count out) 2)) "=="))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn base64Decode [enc]
  (try (do (def alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/") (def bin "") (def i 0) (loop [while_flag_1 true] (when (and while_flag_1 (< i (count enc))) (do (def ch (nth enc i)) (cond (= ch "=") (recur false) :else (do (def idx (indexOf alphabet ch)) (def bin (str bin (toBinary idx 6))) (def i (+ i 1)) (recur while_flag_1)))))) (def out "") (def i 0) (while (<= (+ i 8) (count bin)) (do (def chunk (subs bin i (+ i 8))) (def val (binToInt chunk)) (def out (str out (chr val))) (def i (+ i 8)))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (def msg "Rosetta Code Base64 decode data task")
  (println (str "Original : " msg))
  (def enc (base64Encode msg))
  (println (str "\nEncoded  : " enc))
  (def dec (base64Decode enc))
  (println (str "\nDecoded  : " dec)))

(-main)
