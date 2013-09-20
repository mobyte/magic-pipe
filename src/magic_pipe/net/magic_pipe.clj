(ns net.mobyte.magic-pipe
  (:require [clojure.walk :refer [prewalk]]))

(defn exit [result var exp continue-fn]
  `(if-let [error# ~exp]
     (merge ~result error#)
     ~(continue-fn)))

(defn- global-sym? [sym]
  (and (symbol? sym)
       (re-find #"^\*.*[^*]$" (name sym))))

(defn- global-sym-> [to]
  (fn [sym & [default]]
    (if (global-sym? sym)
      (-> sym name (.substring 1) to)
      (or default sym))))

(defn- global-sym->sym [sym]
  ((global-sym-> symbol) sym))

(defn- global-sym-col->col [col]
  (prewalk global-sym->sym col))

(defn- global-sym->key [sym]
  ((global-sym-> keyword) sym))

(defn- global-sym->result-elem [result sym]
  ((global-sym-> (fn [sym]
                   `(get ~result ~(keyword sym))))
   sym))

(defn- global-sym->assoc-result [result sym]
  ((global-sym-> (fn [sym]
                   `(assoc ~result ~(keyword sym) ~(symbol sym))
                   ))
   sym result))

(defn- flatten* [col]
  (flatten (prewalk #(if (map? %) (seq %) %) col)))

(defn- global-syms-from-col [col]
  (filter global-sym? (flatten* col)))

(defn- global-col->assoc-result [result col]
  (let [col (if (coll? col) col [col])
        body (map (fn [sym]
                    ((global-sym-> (fn [sym]
                                     `(assoc ~(keyword sym) ~(symbol sym))))
                     sym result))
                  (global-syms-from-col col))]
    `(-> ~result ~@body)))

(defn- replace-global-sym-in-exp [exp result]
  (prewalk (partial global-sym->result-elem
                    result)
           exp))

(defn- replace-global-sym-in-var [var]
  (if (coll? var)
    (prewalk global-sym->sym var)
    (global-sym->sym var)))

(defn- continue [result var exp continue-fn]
  `(let [~(replace-global-sym-in-var var) ~(replace-global-sym-in-exp exp result)
         ~result ~(global-col->assoc-result result var)]
     ~(continue-fn)))

(defn- generate-pipe [result [var exp] & var-exps]
  (letfn [(continue-fn []
            (if (seq var-exps)
              (apply generate-pipe
                     result var-exps)
              result))]
    (case var
      :exit? (exit result var exp continue-fn)
      (continue result var exp continue-fn))))

(defmacro pipe* [in-data & exprs]
  (let [in-data-param `in-data#]
    `(let [~in-data-param ~in-data]
       ~(apply generate-pipe
               in-data-param
               (partition 2 exprs)))))

(defmacro pipe [& exprs]
  `(pipe* {} ~@exprs))

(defmacro pipe-fn [& exprs]
  `(fn [in-data#]
     (pipe* in-data# ~@exprs)))




