(ns net.mobyte.magic-pipe
  (:require [clojure.walk :refer [prewalk]]))

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
  (if (coll? exp)
    (prewalk (partial global-sym->result-elem
                      result)
             exp)
    (global-sym->result-elem result exp)))

(defn- replace-global-sym-in-var [var]
  (if (coll? var)
    (prewalk global-sym->sym var)
    (global-sym->sym var)))

(defn- continue [result var exp continue-fn]
  `(let [~(replace-global-sym-in-var var)
         ~(replace-global-sym-in-exp exp result)
         ~result ~(global-col->assoc-result result var)]
     ~(continue-fn)))

(declare generate-pipe)

(defn exit [level result var exp continue-fn]
  `(let [~result (->
                  ~(apply generate-pipe
                          (inc level)
                          result
                          (partition 2 exp))
                  (assoc :_exit? true))]
     ~(continue-fn)))

(defn- condition [level result var exp continue-fn]
  `(let [~result
         (merge ~result
                (cond ~@(apply concat
                               (for [[c e] (partition 2 exp)]
                                 [(replace-global-sym-in-exp c result)
                                  (apply generate-pipe
                                         (inc level)
                                         result
                                         (partition 2 e))]))))]
     ~(continue-fn)))

(defn- void [result var exp continue-fn]
  `(do ~(replace-global-sym-in-exp exp result)
       ~(continue-fn)))

(defn- clear-flags [result level]
  (if (= level 0)
    `(dissoc ~result :_exit?)
    result))

(defn- generate-pipe [level result [var exp] & var-exps]
  (letfn [(continue-fn []
            `(if (:_exit? ~result)
               ~(clear-flags result level)
               ~(if (seq var-exps)
                  (apply generate-pipe
                         level
                         result
                         var-exps)
                  (clear-flags result level))))]
    (case var
      :exit (exit level result var exp continue-fn)
      :cond (condition level result var exp continue-fn)
      :void (void result var exp continue-fn)
      (continue result var exp continue-fn))))

(defmacro pipe* [in-data & exprs]
  (let [in-data-param `in-data#]
    `(let [~in-data-param ~in-data]
       ~(apply generate-pipe
               0
               in-data-param
               (partition 2 exprs)))))

(defmacro pipe [& exprs]
  `(pipe* {} ~@exprs))

(defmacro pipe-fn [& exprs]
  `(fn [in-data#]
     (pipe* in-data# ~@exprs)))




