# on-lisp-with-clojure

Studying excellent [On Lisp](http://www.paulgraham.com/onlisp.html) with Clojure. It's Prolog on Clojure.

```clojure
user> (with-rules (-> *rules*
                      (<- (append [] Xs Xs))                 ; Defining "append" with Prolog.
                      (<- (append [X & As] Bs [X & Cs])
                          (append As Bs Cs)))
        (into [] (with-inference (append X Y [:a :b :c :d])  ; Querying X Y with Prolog.
                   [X Y])))

[[[] [:a :b :c :d]] [[:a] [:b :c :d]] [[:a :b] [:c :d]] [[:a :b :c] [:d]] [[:a :b :c :d] []]]
```

## License

Copyright © 2015 OJIMA Ryoji

Distributed under the Eclipse Public License either version 1.0 or any later version.
