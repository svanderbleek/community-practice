(ns para
  (:import [java.net URLEncoder]))

(def log (atom []))

(defn url-echo [n]
  (let [pre "{\"headers\":[],\"body\":{\"type\":\"text\",\"data\":\""
        post "\"},\"status\":200}"
        query (str  pre n post)]
  (str "https://echoserver.dev/server?query=" (URLEncoder/encode query))))

(defn fetch! [n]
  (swap! log conj (str "fetch!: " (slurp (url-echo n)))))

(time (doall (pmap fetch! (range 100))))
