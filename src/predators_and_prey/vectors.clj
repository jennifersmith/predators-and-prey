(ns predators-and-prey.vectors)

(defn ortho [[x y]]
  [(- y) x])

(defn add [[u v] [x y]]
  [(+ u x) (+ v y)])

(defn sub [[u v] [x y]]
  [(- u x) (- v y)])

(defn pythag [a b]
	(Math/pow (- a b) 2))
(defn len 
([[x y]] (Math/sqrt (+ (* x x) (* y y))))
([[x y][x1 y1]] (Math/sqrt (+ (pythag x x1) (pythag y y1)))))

(defn unit [[x y]]
  [(/ x (len [x y]))
   (/ y (len [x y]))])

(defn mul [a [x y]]
  [(* a x) (* a y)])

(defn coords [{:keys [x y]}] (list x y))