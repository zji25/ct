(defn sizes [w] (let [x (count (first w))] (every? #(== (count %) x) w)))
(defn is_v [v] (and (vector? v) (every? number? v)))
(defn is_m [m] (and (vector? m) (and (every? is_v m) (sizes m))))
(defn is_x [x] (or (is_v x) (and (every? is_x x) (== (count x) (count (first x)))
 (every? true? (mapv #(== (count (first %)) (last %)) (map vector x (iterate dec (count x))))))))
(defn are_v [v] (and (every? is_v v) (sizes v)))
(defn are_m [m] (and (every? is_m m) (sizes m)))
(defn are_x [x] (and (every? is_x x) (if (is_v (first x)) (are_v x) (= (mapv #(count %) x)))))

(defn opv [f] (fn [& vs] {:pre [(are_v vs)] :post [(is_v %)]} (apply mapv f vs)))
(def v+ (opv +))
(def v- (opv -))
(def v* (opv *))
(def vd (opv /))
(defn scalar [& vs] {:pre [(are_v vs)] :post [(number? %)]}
  (reduce + (reduce v* vs)))
(defn vect [& vs] {:pre [(and (are_v vs) (== (count (first vs)) 3))] :post [(and (is_v %) (== (count %) 3))]}
  (letfn [(z [x i y j] (* (nth x i) (nth y j)))]
  (reduce (fn [a b] (vector (- (z a 1 b 2) (z a 2 b 1)) (- (z a 2 b 0) (z a 0 b 2)) (- (z a 0 b 1) (z a 1 b 0)))) vs)))
(defn v*s [v & ss] {:pre [(and (is_v v) (every? number? ss))] :post [(is_v %)]}
  (let [s (reduce * ss)] (mapv #(* % s) v)))

(defn opm [f] (fn [& ms] {:pre [(are_m ms)] :post [(is_m %)]} (apply mapv f ms)))
(def m+ (opm v+))
(def m- (opm v-))
(def m* (opm v*))
(def md (opm vd))
(defn m*s [m & ss] {:pre [(and (is_m m) (every? number? ss))] :post [(is_m %)]}
  (let [s (reduce * ss)] (mapv #(v*s % s) m)))
(defn m*v [m & vs] {:pre [(and (is_m m) (are_v vs) (== (count (first m)) (count (first vs))))] :post [(is_v %)]}
  (reduce (fn [m v] (mapv #(scalar % v) m)) m vs))
(defn transpose [m] {:pre [(is_m m)] :post [(is_m %)]}
  (apply mapv vector m))
(defn m*m [& ms] {:pre [(every? is_m ms)] :post [(is_m %)]}
  (reduce (fn [a b] {:pre [(== (count (first a)) (count b))]}
  (let [t (transpose b)] (mapv (fn [x] (mapv #(scalar % x) t)) a))) ms))

(defn opx [f] (letfn [(r [& xs] (if (every? is_v xs) (apply f xs) (apply mapv r xs)))]
  (fn [& xs] {:pre [(are_x xs)] :post [(is_x %)]} (apply r xs))))
(def x+ (opx v+))
(def x- (opx v-))
(def x* (opx v*))
(def xd (opx vd))