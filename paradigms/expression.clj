(use '[clojure.string :only (join)])
(defn division ([] 1) ([a] (/ 1.0 a)) ([a & b] (/ a (double (apply * b)))))
(defn average  ([] 0) ([& w] (/ (apply + w) (count w))))
(defn variance ([] 0) ([& w] (let [sq #(* % %)] (- (apply average (map sq w)) (sq (apply average w))))))
(defn parse [op] (letfn [(p [e] (cond (list? e) (apply (op (first e)) (map p (rest e)))
                                      (symbol? e) ((op 'v) (str e)) :else ((op 'c) e)))]
                   #(p (read-string %))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def constant constantly)
(defn variable [xyz] #(get % xyz))
(defn op [f] (fn [& ws] (fn [m] (apply f (map #(% m) ws)))))
(def negate (op -))
(def add (op +))
(def subtract negate)
(def multiply (op *))
(def divide (op division))
(def mean (op average))
(def varn (op variance))

(def op-fun {'v variable 'c constant 'negate negate '+ add '- subtract '* multiply '/ divide 'mean mean 'varn varn})
(def parseFunction (parse op-fun))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare zero Multiply Divide)
(definterface make-expr
 (^Number f [mp]) (^Object d [xyz])
 (^String sp []) (^String ss []) (^String si []))
(defn evaluate [exp xyz] (.f exp xyz))
(defn diff [exp xyz] (.d exp xyz))
(defn toString [exp] (.sp exp))
(defn toStringSuffix [exp] (.ss exp))
(defn toStringInfix [exp] (.si exp))

(deftype cns-t [n] make-expr
 (f [th _] (.n th)) (d [_ _] zero)
 (sp [th] (str (.n th))) (ss [th] (.sp th)) (si [th] (.sp th)))
(def Constant #(cns-t. %))
(def zero (Constant 0))
(def one (Constant 1))
(def two (Constant 2))

(deftype var-t [xyz] make-expr
 (f [th mp] (mp (.xyz th))) (d [th dx] (if (= dx (.xyz th)) one zero))
 (sp [th] (.xyz th)) (ss [th] (.sp th)) (si [th] (.sp th)))
(def Variable #(var-t. %))

(deftype opr-t [symbol f d args] make-expr
 (f [th xyz] (apply (.f th) (map #(.f % xyz) (.args th))))
 (d [th xyz] ((.d th) (.args th) (map #(.d % xyz) (.args th))))
 (sp [th] (str "(" (.symbol th) " " (join " " (map #(.sp %) (.args th))) ")"))
 (ss [th] (str "(" (join " " (map #(.ss %) (.args th))) " " (.symbol th) ")"))
 (si [th] (let [s (str " " (.symbol th) " ")] (str "(" (join s (map #(.si %) (.args th))) ")"))))
(defn make-op [symbol f d] #(opr-t. symbol f d %&))

(def Add (make-op '+ + #(apply Add %2)))
(def Subtract (make-op '- - #(apply Subtract %2)))
(def Negate (make-op 'negate - #(if (= (count %1) 1) (apply Negate %2) (apply Subtract %2))))
(defn mul' [w wi] (if (some #(and (instance? cns-t %) (= (.n %) 0)) w) zero
                      (apply Multiply (conj w (apply Add (map Divide wi w)))))) ; (abc)' = abc * (a'/a + b'/b + c'/c)
(def Multiply (make-op '* * mul'))
(def Divide (make-op '/ division
 (fn [w wi] (let [length (count w)]
  (cond (= length 0) one (= length 1) (apply Divide (apply Negate wi) (map Multiply w w))
   :else (let [rw (rest w) m-rw (apply Multiply rw)]
    (Divide (Subtract (Multiply (first wi) m-rw) (Multiply (first w) (mul' rw (rest wi)))) (Multiply m-rw m-rw))))))))
(def Mean (make-op 'mean average #(if (= (count %2) 0) zero (apply Mean %2))))
(def Varn (make-op 'varn variance
 (fn [w wi] (if (= (count w) 0) zero
  (Multiply two (Subtract (apply Mean (map Multiply w wi)) (Multiply (apply Mean w) (apply Mean wi))))))))

(def op-obj {'v Variable 'c Constant 'negate Negate '+ Add '- Subtract '* Multiply '/ Divide 'mean Mean 'varn Varn})
(def parseObject (parse op-obj))