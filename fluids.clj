(import '(java.awt Color Graphics RenderingHints Graphics2D)
        '(java.awt.image BufferedImage)
        '(java.awt.event MouseListener)
        '(javax.swing JFrame JPanel))

(set! *warn-on-reflection* false)

;; Helpers

(defn nth! [s idx] (nth s (mod idx (count s))))

(defn IX [obj x y]
  (if (or (neg? x)
	  (neg? y)
	  (>= x (-> obj first count))
	  (>= y (-> obj count)))
    (if (seq (ffirst obj)) [0 0] 0)
    (-> (obj y) x)))

(declare diffuse-board)

(def *dt* 1.0)

(defn make-densities [w h]
  (vec (map vec (partition h (vec (repeat (* w h) 0))))))

(defn make-velocities [w h]
  (vec (map vec (partition h (vec (repeat (* w h) [0 -1]))))))

(defn show-board [b]
  (doseq [row b]
    (doseq [cell row]
      (print (format "%2.2f  " cell)))
    (println)))

(defn diff-test [n board]
  (println "-[:init]---------------------------------------------------")
  (show-board board)
  (println "-------------------------------------------------[:done]---")
  (loop [n n board board]
    (let [next-board (diffuse-board board 2 2)]
      (when (pos? n)
	(show-board next-board)
	(println "-----------------------------------------------------------")
	(recur (dec n) next-board)))))

;; Sources

(defn add-sources
  [board sources]
  (loop [source sources next-board (transient board)]
    (if-not source
      (with-meta (persistent! next-board) (meta board))
      (let [s (if (seq source) (first source) source)]
	(assoc! next-board (s 1) (assoc (next-board (s 1)) (s 0) 255.0))
	(recur (next source) next-board)))))
	
;; Diffusion

(defn diffuse-row
  [dr above row below]
  (let [row (transient row)]
    (dotimes [idx (count row)]
      (assoc! row idx
	      (let [above (nth! above idx)
		    left  (nth! row   (dec idx))
		    right (nth! row   (inc idx))
		    below (nth! below idx)]
		(if (and (zero? left) (zero? right)
			 (zero? above) (zero? below))
		  0
		  (/ (+ (row idx)
			(* dr (+ (+ above below) (+ left right))))
		     (inc (* 4 dr)))))))
    (persistent! row)))

(defn stabilize [max f]
  (loop [n max state (f)]
    (let [next-state (f)]
      (if (or (zero? n) (= state next-state))
	state
	(recur (dec n) next-state)))))

(defn diffuse-board
  [board diffusion]
  (let [diffusion-rate   (* *dt* diffusion (count board))]
      (vec (map (fn [[above row below]]
		  (stabilize 20 #(diffuse-row diffusion-rate above row below)))
	(partition 3 1 (concat [(peek board)] board [(first board)]))))))

(defn advection [densities velocities]
  (let [[w h] [(count densities) (-> densities first count)]
	board (transient densities)]
      (dotimes [i h]
	(dotimes [j w]
	  (let [velocity    ((velocities i) j)
		x           (- i (* *dt* (velocity 0)))
		x           (cond
			     (< x 0.5)        0.5
			     (> x (+ w 0.5)) (+ w 0.5)
			     :else x)
		y           (- j (* *dt* (velocity 1)))
		y           (cond
			     (< y 0.5)        0.5
			     (> y (+ w 0.5))  (+ w 0.5)
			     :else y)
		i0          (int x)
		i1          (+ i0 1)
		
		j0          (int y)
		j1          (+ j0 1)

		s1          (- x i0)
		s0          (- 1 s1)

		t1          (- y j0)
		t0          (- 1 t1)
		old-density (nth! (nth! densities i) j)
		new-density (+  (* s0 (+ (* t0 (nth! (nth! densities j0) i0))
					 (* t1 (nth! (nth! densities j0) i1))))
				(* s1 (+ (* t0 (nth! (nth! densities j1) i0))
					 (* t1 (nth! (nth! densities j1) i1)))))]
	    (assoc! board j (assoc (nth! board j) i
				   (min 255 new-density))))))
      (persistent! board)))

(defn project [densities velocities]
  (...

;; Rendering

(def running (atom true))

(defn render-scene [#^Graphics2D g w h scale board]
  (doto g
    (.setColor Color/BLACK)
    (.fillRect 0 0 (* scale w) (* scale h)))
  (doseq [i (range h) j (range w)]
    (let [density (int ((board i) j))]
      (doto g
	(.setColor (Color. density density density))
	(.fillRect (* j scale) (* i scale) scale scale)))))

(defmacro defworker [name args & body]
  " Defines a worker which discontinues looping his main body
    if the global atom 'running' is non-true. Exceptions are
    caught and printed "
  `(defn ~name ~args
     (while @running
	    (try ~@body (catch Exception e#
			  (-> e# .getMessage println))))))

(defworker animator [panel]
  (.repaint panel)
  (Thread/sleep 40))

(defworker dynamics [densities velocities]
  (dotimes [i 5]
    (swap! densities
	   add-sources [[(+ (- 5 (rand-int 10)) (/ (count @densities) 2))
			 (- (count @densities) 2)]]))
  (swap! densities diffuse-board 2)
  (swap! densities advection @velocities))

(defn main [scale [w h]]
  (reset! running true)
  (let [densities  (atom (make-densities  w h))
	velocities (atom (make-velocities w h))
	panel (doto (proxy [JPanel] []
		      (paint [g] (render-scene g w h scale @densities))))]
    (doto (JFrame. "Fluid Dynamics")
      (.addWindowListener
       (proxy [java.awt.event.WindowAdapter] []
	 (windowClosing [_] (reset! running false))))
      (.setSize (* scale w) (* scale  h))
      (.setUndecorated true)
      .pack .show
      (.setLocation 400 200)
      (.add panel))
    (future (animator panel))
    (future (dynamics densities velocities))))