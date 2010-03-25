(import '(java.awt Color Graphics RenderingHints Graphics2D)
        '(java.awt.image BufferedImage)
        '(java.awt.event MouseListener)
        '(javax.swing JFrame JPanel))

(set! *warn-on-reflection* false)

;; Helpers

(defmacro aget!
  ([hint array idx]
     `(aget ~(vary-meta array assoc :tag hint) ~idx))
  ([hint array idx & idxs]
     `(let [a# (aget ~(vary-meta array assoc :tag 'objects) ~idx)]	
	(aget! ~hint a# ~@idxs))))

(defmacro aset! [hint array & idxsv]
  (let [hints '{doubles double ints int}
        [v idx & sxdi] (reverse idxsv)
        idxs (reverse sxdi)
        v (if-let [h (hints hint)] (list h v) v)
        nested-array (if (seq idxs)
                       `(aget! ~'objects ~array ~@idxs)
                        array)
        a-sym (with-meta (gensym "a") {:tag hint})]
    `(let [~a-sym ~nested-array]
       (aset ~a-sym ~idx ~v))))

(defmacro do-board [[w h] & body]
  `(let [w# (int (dec ~w)) h# (int (dec ~h))]
     (loop [~'i (int 1)]
       (when (< ~'i w#)
	 (loop [~'j (int 1)]
	   (when (< ~'j h#)
	     (do ~@body)
	     (recur (unchecked-inc ~'j))))
	 (recur (unchecked-inc ~'i))))))

(defmacro map! [f lookup collection]
  `(let [[w# h#] [(count ~lookup) (-> ~lookup first count)]]
     (let [board# (make-array Double/TYPE h# w#)]
       (do-board [w# h#]       
	    (let [~'above  (aget! ~'doubles ~lookup (unchecked-dec ~'i) ~'j)
		  ~'below  (aget! ~'doubles ~lookup (unchecked-inc ~'i) ~'j)
		  ~'left   (aget! ~'doubles ~lookup ~'i (unchecked-dec ~'j))
		  ~'right  (aget! ~'doubles ~lookup ~'i (unchecked-inc ~'j))
		  ~'self   (aget! ~'doubles ~lookup ~'i ~'j)]
	      (aset! ~'doubles board# (int ~'i) (int ~'j) ~f)))
       board#)))

(declare diffuse-board)

(def *dt* 1.0)

(defn make-densities [w h]
  (vec (map vec (partition h (vec (repeat (* w h) 0))))))

(defn make-densities [w h]
  (let [board (make-array Double/TYPE h w)]
    (dotimes [i h]
      (dotimes [j w]
	(aset! doubles board i j 0)))
    board))

(defn new-array [w h]
  (let [array (make-array Integer/TYPE h w)]
    (do-board [w h] (aset! ints array i j 0))
    array))

(defn topdown-array [orig]
  (let [[u v] orig
	[w h] [(count v) (-> v first count)]]
    (do-board [w h]  (aset! ints v i j (if (>= j (/ h 2)) -1 1)))
    [u v]))

(defn randr-array [orig]
  (let [[u v] orig
	[w h] [(count v) (-> v first count)]]
    (do-board [w h]
	      (aset! ints u i j (rand-int 2))
	      (aset! ints v i j (rand-int 2)))
    [u v]))

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
  (doseq [[x y] sources] (aset! doubles board x y 255.0))
  board)
	
;; Diffusion

(defn stabilize [max f]
  (loop [n max state (f)]
    (let [next-state (f)]
      (if (or (zero? n) (= state next-state))
	state
	(recur (dec n) next-state)))))

(defn diffuse-board
  [board diffusion]
  (let [sz         (count board)
	dr         (* *dt* diffusion sz sz)
	dconst     (* 4 dr)]
    (map! (/ (+ self (* dr (+ (+ above below) (+ left right)))) dconst)
	  board (make-array Double/TYPE sz sz))))

(defn advection [densities velocities]
  (let [[w h] [(count densities) (-> densities first count)]
	[u v] velocities
	retr  (make-densities w h)
	[w h] [(unchecked-dec w) (unchecked-dec h)]]
    (do-board [w h]
      (let [x  (- i (* *dt* (aget! ints u i j)))
	    y  (- j (* *dt* (aget! ints v i j)))
	    x  (cond (< x 0.5)       0.5
		     (> x (+ 0.5 w)) (+ 0.5 w)
		     :else x)
	    y  (cond (< y 0.5)       0.5
		     (> y (+ 0.5 h)) (+ 0.5 (unchecked-dec h))
		     :else y)

	    i0 (int x)
	    i1 (unchecked-inc i0)

	    j0 (int y)
	    j1 (unchecked-inc j0)

	    s1 (int (- x i0))
	    s0 (int (- 1 s1))

	    t1 (int (- y j0))
	    t0 (int (- 1 t1))]
	(aset! doubles retr i j
		   (+ (* s0 (+ (* t0 (aget! doubles densities i0 j0))
			       (* t1 (aget! doubles densities i0 j1))))
		      (* s1 (+ (* t0 (aget! doubles densities i1 j0))
			       (* t1 (aget! doubles densities i1 j1))))))))
    retr))		    

(defn set-bounds [velocities b idx]
  velocities)		

(defn project [velocities]
  (let [[w h]    [(count velocities) (-> velocities first count)]
	[u v]    velocities
	r        (/ 1.0 h)
	p        (make-densities w h)
	div      (make-densities w h)]
    (do-board [w h]
       (aset! doubles div i j
		  (* -0.5 r (+ (- (aget! ints u (unchecked-inc i) j)
				  (aget! ints u (unchecked-dec i) j))
			       (- (aget! ints v i (unchecked-inc j))
				  (aget! ints v i (unchecked-dec j)))))))
    (dotimes [k 20]
    (do-board [w h]
      (aset! doubles p
		 (/ (+ (aget! doubles div i j)
		       (aget! doubles p (unchecked-dec i) j)
		       (aget! doubles p (unchecked-inc i) j)
		       (aget! doubles p i (unchecked-dec j))
		       (aget! doubles p i (unchecked-inc j))) 4))))
    (do-board [w h]
      (aset! ints u i j
	     (- (aget! ints u i j)
		(/ (* 0.5 (- (aget! doubles p (unchecked-inc i) j)
			     (aget! doubles p (unchecked-dec i) j)))
		   r)))
      (aset! ints v i j
	     (- (aget! ints v i j)
		(/ (* 0.5 (- (aget! doubles p i (unchecked-inc j))
			     (aget! doubles p i (unchecked-dec j))))
		   r))))
    [u v]))
    
;; Rendering

(def running (atom true))
(def show-velocities (atom true))

(defn render-scene [#^Graphics2D g w h scale board velocities]
  (doto g
    (.setColor Color/BLACK)
    (.fillRect 0 0 (* scale w) (* scale h)))
  (do-board [w h]
    (let [density (min 255 (int (aget! doubles board i j)))]
      (.setColor g (Color. density density density))
      (.fillRect g (* i scale) (* j scale) scale scale)
      (when @show-velocities
	(let [[u v] velocities
	      mx    (+ (* i scale) (/ scale 2)) ; Middle of cell, X
	      my    (+ (* j scale) (/ scale 2))
	      ux    (aget! ints u i j)
	      vy    (aget! ints v i j)] 
	  (if (pos? vy)
	    (.setColor g Color/RED)
	    (.setColor g Color/GREEN))
	(.drawLine g mx my (dec (+ mx (* ux scale)))
		           (dec (+ my (* vy scale)))))))))

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

(defworker density-step [densities velocities sources panel]
  (when-let [s (seq @sources)] (swap! densities add-sources s))
  (swap! densities diffuse-board 4)
  (swap! densities advection @velocities)

  (when-let [s (seq @sources)] (swap! densities add-sources s))
  (swap! densities diffuse-board 4)
  (swap! velocities project)
  (swap! densities advection @velocities)
  (swap! velocities project))
  
(defn new-source [e scale sources]
  (swap! sources conj [(int (/ (.getX e) scale)) (int (/ (.getY e) scale))]))

(defn main [scale [w h]]
  (reset! running true)
  (let [densities  (atom (make-densities  w h))
	d0         (make-array Double/TYPE w h)
	velocities (atom (randr-array [(new-array w h) (new-array w h)]))
	sources    (atom [])
	panel (doto (proxy [JPanel] []
		      (paint [g] (render-scene g w h scale
					       @densities
					       @velocities))))]
    (doto (JFrame. "Fluid Dynamics")
      (.addWindowListener   (proxy [java.awt.event.WindowAdapter] []
	 (windowClosing [_] (reset! running false))))
      (.addMouseListener    (proxy [java.awt.event.MouseListener] []
	 (mouseExited     [e] nil)
	 (mouseReleased   [e] nil)
	 (mousePressed    [e] nil)
	 (mouseEntered    [e] nil)
	 (mouseClicked    [e] (if (= java.awt.event.MouseEvent/BUTTON1
				     (.getButton e))
				(new-source e scale sources)
				(swap! show-velocities not)))))
      (.setSize (* scale w) (* scale  h))
      (.setUndecorated true)
      .pack .show
      (.setLocation 400 200)
      (.add panel))
    (future (animator panel))
    (future (density-step densities velocities sources panel))
));    (future (velocity-step densities velocities))))