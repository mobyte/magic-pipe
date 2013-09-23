(ns net.mobyte.magic-pipe.examples.square-roots
  (:require [net.mobyte.magic-pipe :refer [pipe pipe-fn]]
            [clojure.pprint :refer [pp pprint]]))

(def log-error (partial println "ERROR: "))
(def log-info (partial println "INFO: "))

(let [a 10]
  (-> {:some-data {:some-in-params {:b 5 :c 2}}}
      ((pipe-fn
        {:keys [a b c] :as *params} (-> *some-data
                                        :some-in-params
                                        (assoc :a a))
        d (- (* b b) (* 4 a c))
        :cond [(= d 0) [*roots [(/ (- b) (* 2.0 a))]]
               (> d 0) [*roots (map #(/ (% (- b) (Math/sqrt d))
                                        (* 2.0 a))
                                    [+ -])]
               :else [*error "no real roots"]]))
      ((pipe-fn
        :cond [*error [:exit
                       [:void
                        (do (log-error "finished w/ errors")
                            (log-error *error))]]]
        :void (do (log-info "params = " *params)
                  (log-info "roots = " *roots))))
      pprint))
