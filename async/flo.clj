(ns flo
  (:require [clojure.core.async.flow :as flow]
            [clojure.core.async :as async]
            [clojure.core.async.flow-monitor :as monitor]
            [clojure.datafy :refer [datafy]]))

(defn source-fn
  ([] {:params {:source-chan "Source"}
       :outs {:out "Output"}})
  ([{:keys [source-chan]}]
   {::flow/in-ports {:in source-chan}})
  ([state status]
   (println state status)
   state)
  ([state input n]
   (println state input n)
   [state {:out [n]}]))

(defn sink-fn
  ([] {:params {:sink-chan "Sink"}
       :ins {:in "Input"}})
  ([{:keys [sink-chan]}]
   {::flow/out-ports {:out sink-chan}})
  ([state status]
   (println state status)
   state)
  ([state input n]
   (println state input n)
   [state {:out [n]}]))

(def in-chan (async/chan))
(def out-chan (async/chan))

(def min-flow (flow/create-flow {:procs {:source-proc {:proc (flow/process #'source-fn)
                                                       :args (:source-chan in-chan)}
                                         :sink-proc   {:proc (flow/process #'sink-fn)
                                                       :args {:sink-chan out-chan}}}
                                 :conns [[[:source-proc :out] [:sink-proc :in]]]}))

(def server-state (monitor/start-server {:flow min-flow :port 9876}))
