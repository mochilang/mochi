(ns main (:refer-clojure :exclude [connect main]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare connect main)

(defn connect [client]
  (try (throw (ex-info "return" {:v (and (not= (:Host client) "") (> (:Port client) 0))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def client {:Base "dc=example,dc=com" :Host "ldap.example.com" :Port 389 :UseSSL false :BindDN "uid=readonlyuser,ou=People,dc=example,dc=com" :BindPassword "readonlypassword" :UserFilter "(uid=%s)" :GroupFilter "(memberUid=%s)" :Attributes ["givenName" "sn" "mail" "uid"]}) (if (connect client) (println (str "Connected to " (:Host client))) (println "Failed to connect"))))

(defn -main []
  (main))

(-main)
