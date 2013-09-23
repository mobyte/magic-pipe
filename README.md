# magic_pipe

Yet another 'pipe' pattern implementation.

## Usage

```clj
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

a = 10:
=>
ERROR:  finished w/ errors
ERROR:  no real roots
{:error "no real roots",
 :params {:a 10, :c 2, :b 5},
 :some-data {:some-in-params {:c 2, :b 5}}}

a = -10:
=>
INFO:  params =  {:a -10, :c 2, :b 5}
INFO:  roots =  (-0.2623475382979799 0.7623475382979799)
{:roots (-0.2623475382979799 0.7623475382979799),
 :params {:a -10, :c 2, :b 5},
 :some-data {:some-in-params {:c 2, :b 5}}}
```

## License

Copyright © 2013 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
