(ns main (:refer-clojure :exclude [splitLines parseFasta main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare splitLines parseFasta main)

(def FASTA (str (str (str (str (str ">Rosetta_Example_1\n" "THERECANBENOSPACE\n") ">Rosetta_Example_2\n") "THERECANBESEVERAL\n") "LINESBUTTHEYALLMUST\n") "BECONCATENATED"))

(defn splitLines [s]
  (try (do (def lines []) (def start 0) (def i 0) (while (< i (count s)) (if (= (subs s i (+ i 1)) "\n") (do (def lines (conj lines (subs s start i))) (def i (+ i 1)) (def start i)) (def i (+ i 1)))) (def lines (conj lines (subs s start (count s)))) (throw (ex-info "return" {:v lines}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn parseFasta [text]
  (try (do (def key "") (def val "") (def out []) (loop [line_seq (splitLines text)] (when (seq line_seq) (let [line (first line_seq)] (cond (= line "") (recur (rest line_seq)) :else (do (if (= (subs line 0 1) ">") (do (when (not= key "") (def out (conj out (str (str key ": ") val)))) (def hdr (subs line 1 (count line))) (def idx 0) (while (and (< idx (count hdr)) (not= (subs hdr idx (+ idx 1)) " ")) (def idx (+ idx 1))) (def key (subs hdr 0 idx)) (def val "")) (do (when (= key "") (do (println "missing header") (throw (ex-info "return" {:v []})))) (def val (str val line)))) (recur (rest line_seq))))))) (when (not= key "") (def out (conj out (str (str key ": ") val)))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def res (parseFasta FASTA)) (doseq [line res] (println line))))

(defn -main []
  (main))

(-main)

