(ns main (:refer-clojure :exclude [shuffleStr createPolybius createKey orderKey encrypt indexOf decrypt main]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(def adfgvx "ADFGVX")

(def alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

(defn shuffleStr [s]
  (try (do (def arr []) (def i 0) (while (< i (count s)) (do (def arr (conj arr (subs s i (+ i 1)))) (def i (+ i 1)))) (def j (- (count arr) 1)) (while (> j 0) (do (def k (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) (+ j 1))) (def tmp (nth arr j)) (def arr (assoc arr j (nth arr k))) (def arr (assoc arr k tmp)) (def j (- j 1)))) (def out "") (def i 0) (while (< i (count arr)) (do (def out (str out (nth arr i))) (def i (+ i 1)))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn createPolybius []
  (try (do (def shuffled (shuffleStr alphabet)) (def labels []) (def li 0) (while (< li (count adfgvx)) (do (def labels (conj labels (subs adfgvx li (+ li 1)))) (def li (+ li 1)))) (println "6 x 6 Polybius square:\n") (println "  | A D F G V X") (println "---------------") (def p []) (def i 0) (while (< i 6) (do (def row (subvec shuffled (* i 6) (* (+ i 1) 6))) (def p (conj p row)) (def line (str (nth labels i) " | ")) (def j 0) (while (< j 6) (do (def line (str (str line (subvec row j (+ j 1))) " ")) (def j (+ j 1)))) (println line) (def i (+ i 1)))) (throw (ex-info "return" {:v p}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn createKey [n]
  (try (do (when (or (< n 7) (> n 12)) (println "Key should be within 7 and 12 letters long.")) (def pool "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789") (def key "") (def i 0) (while (< i n) (do (def idx (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) (count pool))) (def key (str key (nth pool idx))) (def pool (+ (subs pool 0 idx) (subs pool (+ idx 1) (count pool)))) (def i (+ i 1)))) (println (str "\nThe key is " key)) (throw (ex-info "return" {:v key}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn orderKey [key]
  (try (do (def pairs []) (def i 0) (while (< i (count key)) (do (def pairs (conj pairs [(subs key i (+ i 1)) i])) (def i (+ i 1)))) (def n (count pairs)) (def m 0) (while (< m n) (do (def j 0) (while (< j (- n 1)) (do (when (> (nth (nth pairs j) 0) (nth (nth pairs (+ j 1)) 0)) (do (def tmp (nth pairs j)) (def pairs (assoc pairs j (nth pairs (+ j 1)))) (def pairs (assoc pairs (+ j 1) tmp)))) (def j (+ j 1)))) (def m (+ m 1)))) (def res []) (def i 0) (while (< i n) (do (def res (conj res (int (nth (nth pairs i) 1)))) (def i (+ i 1)))) (throw (ex-info "return" {:v res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn encrypt [polybius key plainText]
  (try (do (def labels []) (def li 0) (while (< li (count adfgvx)) (do (def labels (conj labels (subs adfgvx li (+ li 1)))) (def li (+ li 1)))) (def temp "") (def i 0) (while (< i (count plainText)) (do (def r 0) (while (< r 6) (do (def c 0) (while (< c 6) (do (when (= (subvec (nth polybius r) c (+ c 1)) (subs plainText i (+ i 1))) (def temp (str (str temp (subvec labels r (+ r 1))) (subvec labels c (+ c 1))))) (def c (+ c 1)))) (def r (+ r 1)))) (def i (+ i 1)))) (def colLen (/ (count temp) (count key))) (when (> (mod (count temp) (count key)) 0) (def colLen (+ colLen 1))) (def table []) (def rIdx 0) (while (< rIdx colLen) (do (def row []) (def j 0) (while (< j (count key)) (do (def row (conj row "")) (def j (+ j 1)))) (def table (conj table row)) (def rIdx (+ rIdx 1)))) (def idx 0) (while (< idx (count temp)) (do (def row (/ idx (count key))) (def col (mod idx (count key))) (def table (assoc-in table [row col] (subs temp idx (+ idx 1)))) (def idx (+ idx 1)))) (def order (orderKey key)) (def cols []) (def ci 0) (while (< ci (count key)) (do (def colStr "") (def ri 0) (while (< ri colLen) (do (def colStr (str colStr (nth (nth table ri) (nth order ci)))) (def ri (+ ri 1)))) (def cols (conj cols colStr)) (def ci (+ ci 1)))) (def result "") (def ci 0) (while (< ci (count cols)) (do (def result (str result (nth cols ci))) (when (< ci (- (count cols) 1)) (def result (str result " "))) (def ci (+ ci 1)))) (throw (ex-info "return" {:v result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn indexOf [s ch]
  (try (do (def i 0) (while (< i (count s)) (do (when (= (subs s i (+ i 1)) ch) (throw (ex-info "return" {:v i}))) (def i (+ i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn decrypt [polybius key cipherText]
  (try (do (def colStrs []) (def start 0) (def i 0) (while (<= i (count cipherText)) (do (when (or (= i (count cipherText)) (= (nth cipherText i) " ")) (do (def colStrs (conj colStrs (subs cipherText start i))) (def start (+ i 1)))) (def i (+ i 1)))) (def maxColLen 0) (def i 0) (while (< i (count colStrs)) (do (when (> (count (nth colStrs i)) maxColLen) (def maxColLen (count (nth colStrs i)))) (def i (+ i 1)))) (def cols []) (def i 0) (while (< i (count colStrs)) (do (def s (nth colStrs i)) (def ls []) (def j 0) (while (< j (count s)) (do (def ls (conj ls (subvec s j (+ j 1)))) (def j (+ j 1)))) (if (< (count s) maxColLen) (do (def pad []) (def k 0) (while (< k maxColLen) (do (if (< k (count ls)) (def pad (conj pad (nth ls k))) (def pad (conj pad ""))) (def k (+ k 1)))) (def cols (conj cols pad))) (def cols (conj cols ls))) (def i (+ i 1)))) (def table []) (def r 0) (while (< r maxColLen) (do (def row []) (def c 0) (while (< c (count key)) (do (def row (conj row "")) (def c (+ c 1)))) (def table (conj table row)) (def r (+ r 1)))) (def order (orderKey key)) (def r 0) (while (< r maxColLen) (do (def c 0) (while (< c (count key)) (do (def table (assoc-in table [r (nth order c)] (nth (nth cols c) r))) (def c (+ c 1)))) (def r (+ r 1)))) (def temp "") (def r 0) (while (< r (count table)) (do (def j 0) (while (< j (count (nth table r))) (do (def temp (str temp (nth (nth table r) j))) (def j (+ j 1)))) (def r (+ r 1)))) (def plainText "") (def idx 0) (while (< idx (count temp)) (do (def rIdx (indexOf adfgvx (subs temp idx (+ idx 1)))) (def cIdx (indexOf adfgvx (subs temp (+ idx 1) (+ idx 2)))) (def plainText (str plainText (nth (nth polybius rIdx) cIdx))) (def idx (+ idx 2)))) (throw (ex-info "return" {:v plainText}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def plainText "ATTACKAT1200AM") (def polybius (createPolybius)) (def key (createKey 9)) (println (str "\nPlaintext : " plainText)) (def cipherText (encrypt polybius key plainText)) (println (str "\nEncrypted : " cipherText)) (def plainText2 (decrypt polybius key cipherText)) (println (str "\nDecrypted : " plainText2))))

(defn -main []
  (main))

(-main)
