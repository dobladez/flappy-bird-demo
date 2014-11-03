(ns flappy-bird-demo.macros)


(defmacro spy[x]
  `(let [x# ~x]
     (prn "spy:" '~x "=" x#)
     x#))
