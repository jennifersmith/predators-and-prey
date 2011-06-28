(ns predators-and-prey.simulation
	(:use predators-and-prey.constants)
	(:use predators-and-prey.vectors)
	(:use predators-and-prey.collisions))

(def predator {:max-velocity 7 :radius 10})
(def prey {:max-velocity 4 :radius 5})

(def animals (atom {}))

(defn create-predator [x y horizontal-velocity vertical-velocity]
	(conj {:x x :y y :vx horizontal-velocity :vy vertical-velocity} predator))
	
(defn create-prey
	([screen-size]
	(conj {:x (rand-int screen-size) :y (rand-int screen-size) :vx (rand-int (:max-velocity prey)) :vy (rand-int (:max-velocity prey))} prey))
	([x y vx vy]
	(conj {:x x :y y :vx vx :vy vy} prey)))
	
(defn prey-generator [screen-size]
	#(conj {:x (rand-int screen-size) :y (rand-int screen-size) :vx (rand-int (:max-velocity prey)) :vy (rand-int (:max-velocity prey))} prey))
	
(defn initial-state []
	{:predators [(create-predator 50 50 4 4)
	(create-predator (- screen-size 100) (- screen-size 100) -4 -4)]
	:prey (take 50 (repeatedly (prey-generator screen-size)))})

(defn move [animal target]
	(let [x (:x animal) y (:y animal)
	new-velocity (sub target (coords animal))
	targetx (:x target) targety (:y target)
	[vx vy] new-velocity]
	(assoc animal :x (mod (+ x vx) screen-size) :y (mod (+ y vy) screen-size))))

(defn target [predator prey]
	(coords (find-prey predator prey)))

(defn surviving? [predators]
	(fn [prey]
		(if (nil? (some #(collides? prey %) predators)) true false)))

(defn find-prey [predator prey]
	(first(sort-by (fn [prey] (len (coords prey) (coords predator))) prey)))

(defn think [current-state]
	(let [new-predators (map (fn [predator] (move predator (target predator (:prey current-state)))) (:predators current-state))
		remaining-prey (filter (surviving? new-predators) (:prey current-state))]
	( -> @animals
		(assoc :predators new-predators :prey remaining-prey))))

(defn pulse []
	(let [bounded-screen-size (- screen-size 20)]
	(if (empty? @animals)
		(reset! animals (initial-state))
		(swap! animals think))))
