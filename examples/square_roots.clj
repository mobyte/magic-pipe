(ns net.mobyte.magic-pipe.examples.square-roots
  (:require [net.mobyte.magic-pipe :refer [pipe pipe-fn]]))

(defn roots [a b c]
  (map #(/ (% (- b) (Math/sqrt (- (* b b) (* 4.0 a c))))
           (* 2.0 a))
       [+ -]))

(defn square-roots []
  (let [a 1]
   (-> {:in-data {:params {:b 5
                           :c 2}}}
       ((pipe-fn
         {:keys [*params]} *in-data))
       ((pipe-fn
         {:keys [b c]} *params
         d (- (* b b) (* 4 a c))
         :exit? (when (< d 0)
                  {:error "no real roots"})
         [*root1 *root2 :as *roots] (roots a b c))))))

;; (square-roots)
;; => {:roots (-0.4384471871911697 -4.561552812808831),
;;     :root2 -4.561552812808831,
;;     :root1 -0.4384471871911697,
;;     :params {:c 2, :b 5},
;;     :in-data {:params {:c 2, :b 5}}}
