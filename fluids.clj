(import '(java.awt Color Graphics RenderingHints Graphics2D)
        '(java.awt.image BufferedImage)
        '(java.awt.event MouseListener)
        '(javax.swing JFrame JPanel))

(set! *warn-on-reflection* false)

;; Helpers

;(defn aset! [arr #^Integer x #^Integer y v]
 ; (let [#^doubles f (aget #^objects arr x)]
  ;  (aset f y (double v))))

(defmacro aget!
  ([array y]      `(aget ~(vary-meta array assoc :tag 'doubles) ~y))
  ([array x & ys] `(let [a# (aget ~(vary-meta array assoc :tag 'objects) ~@ys)]
		     (aget! a# ~x))))

(defmacro aset! [array x y v]
  (let [nested-array `(aget ~(vary-meta array assoc :tag 'objects) ~y)
        a-sym         (with-meta (gensym "a") {:tag 'doubles})]
    `(let [~a-sym ~nested-array]
       (aset ~a-sym ~x (double ~v)))))

(defn new-array [w h]
  (let [board (make-array Double/TYPE h w)]
    (dotimes [i w]
      (dotimes [j h]
	(aset! board i j 0.0)))
    board))

(defmacro do-board [[w h] & body]
  `(let [w# (int (dec ~w)) h# (int (dec ~h))]
     (loop [~'i (int 1)]
       (when (< ~'i w#)
	 (loop [~'j (int 1)]
	   (when (< ~'j h#)
	     (do ~@body)
	     (recur (unchecked-inc ~'j))))
	 (recur (unchecked-inc ~'i))))))
(declare diffuse-board)

(def *dt* 1.0)


(defn up-array [orig]
  (let [[u v] orig
	[w h] [(count v) (-> v first count)]]
    (do-board [w h]  (aset! v i j -0.5))
    [u v]))

(defn topdown-array [orig]
  (let [[u v] orig
	[w h] [(count v) (-> v first count)]]
    (do-board [w h]  (aset! v i j (if (>= j (/ h 2)) -0.5 0.5)))
    [u v]))

(defn randr-array [orig]
  (let [[u v] orig
	[w h] [(count v) (-> v first count)]]
    (do-board [w h]
	      (aset! u i j (- 1.0 (rand 2)))
	      (aset! v i j (- 1.0 (rand 2))))
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
  (doseq [[x y] sources] (aset! board x y 255.0))
  board)
	
;; Diffusion

(defn set-bounds [arr b]
  (let [N      (int (- (count arr) (int 2)))
	n1     (unchecked-inc N)	
	cswap! (fn [#^Integer x #^Integer y old p]
		 (aset! arr x y (if p (- old) old)))]
    (loop [i (int 1)]
      (when (<= i N)
	(cswap! 0  i (aget! arr 1 i) (= b 1))
	(cswap! n1 i (aget! arr N i) (= b 1))
	(cswap! i  0 (aget! arr i 1) (= b 2))
	(cswap! i n1 (aget! arr i N) (= b 2))
	(recur (unchecked-inc i))))
    (aset! arr (int 0) (int 0)
	   (* 0.5 (+ (aget! arr 1 0)
		     (aget! arr 0 1))))
    (aset! arr (int 0) n1
	    (* 0.5 (+ (aget! arr 1 n1)
		      (aget! arr 0 N))))
    (aset! arr n1 (int 0)
	    (* 0.5 (+ (aget! arr N 0)
		      (aget! arr n1 1))))
    (aset! arr n1 n1
	    (* 0.5 (+ (aget! arr N n1)
		      (aget! arr n1 N))))
    arr))

(defmacro map! [f lookup collection]
  `(let [[w# h#] [(count ~lookup) (-> ~lookup first count)]]
     (let [board# (make-array Double/TYPE h# w#)]
       (dotimes [k# (int 20)]
	 (do-board [w# h#]       
	    (let [~'above  (aget! ~lookup (unchecked-dec ~'i) ~'j)
		  ~'below  (aget! ~lookup (unchecked-inc ~'i) ~'j)
		  ~'left   (aget! ~lookup ~'i (unchecked-dec ~'j))
		  ~'right  (aget! ~lookup ~'i (unchecked-inc ~'j))
		  ~'self   (aget! ~lookup ~'i ~'j)]
	      (aset! board# (int ~'i) (int ~'j) ~f)))
	 (set-bounds board# 0))
       board#)))

(defn diffuse-board
  [board b diffusion]
  (let [sz         (count board)
	dr         (* *dt* diffusion sz sz)
	dconst     (* 4 dr)
	d1         (new-array sz sz)]
    (map! (/ (+ self (* dr (+ (+ above below) (+ left right)))) dconst)
	  board d1)))

(defn advection [densities velocities b]
  (let [[w h] [(count densities) (-> densities first count)]
	[u v] velocities
	retr  (new-array w h)
	[w h] [(unchecked-dec w) (unchecked-dec h)]]
    (do-board [w h]
      (let [x  (- i (* *dt* (aget! u i j)))
	    y  (- j (* *dt* (aget! v i j)))
	    x  (cond (< x 0.5)       0.5
		     (> x (+ 0.5 w)) (+ 0.5 (unchecked-dec w))
		     :else x)
	    y  (cond (< y 0.5)       0.5
		     (> y (+ 0.5 h)) (+ 0.5 (unchecked-dec h))
		     :else y)
	    i0  (int x)
	    i1  (unchecked-inc i0)
	    j0  (int y)
	    j1  (unchecked-inc j0)
	    s1  (- x i0)
	    s0  (- 1 s1)
	    t1  (- y j0)
	    t0  (- 1 t1)]
	(aset! retr i j
		   (+ (* s0 (+ (* t0 (aget! densities i0 j0))
			       (* t1 (aget! densities i0 j1))))
		      (* s1 (+ (* t0 (aget! densities i1 j0))
			       (* t1 (aget! densities i1 j1))))))))
    (set-bounds retr b)
    retr))

      
(defn project [velocities]
  (let [[u v]    velocities
	[w h]    [(count u) (-> u first count)]
	r        (/ 1.0 h)
	p        (new-array w h)
	div      (new-array w h)
	half     (double 0.5)]
    (do-board [w h]
       (aset! div i j
		  (* -0.5 r (+ (- (aget! u (unchecked-inc i) j)
				  (aget! u (unchecked-dec i) j))
			       (- (aget! v i (unchecked-inc j))
				  (aget! v i (unchecked-dec j)))))))
    (set-bounds div 0)
    (set-bounds p 0)
    (dotimes [k 20]
      (do-board [w h]
		(aset! p i j
		 (/ (+ (+ (aget! div i j)
			  (aget! p (unchecked-dec i) j))
		       (+ (aget! p (unchecked-inc i) j)
			  (aget! p i (unchecked-dec j)))
		       (aget! p i (unchecked-inc j))) (int 4))))
      (set-bounds p 0))
    (do-board [w h]
      (aset! u i j
	     (- (aget! u i j)
		(/ (* half (- (aget! p (unchecked-inc i) j)
			      (aget! p (unchecked-dec i) j)))
		   r)))
      (aset! v i j
	     (- (aget! v i j)
		(/ (* half (- (aget! p i (unchecked-inc j))
			     (aget! p i (unchecked-dec j))))
		   r))))
    (set-bounds u 1)
    (set-bounds v 2)
    [u v]))
    
;; Rendering

(def running (atom true))
(def show-velocities (atom true))

(defn render-scene [#^Graphics2D g w h scale board velocities]
  (doto g
    (.setColor Color/BLACK)
    (.fillRect 0 0 (* scale w) (* scale h)))
  (do-board [w h]
    (let [density (min 255 (int (aget! board i j)))]
      (.setColor g (Color. density density density))
      (.fillRect g (* i scale) (* j scale) scale scale)
      (when @show-velocities
	(let [[u v] velocities
	      mx    (+ (* i scale) (/ scale 2)) ; Middle of cell, X
	      my    (+ (* j scale) (/ scale 2))
	      ux    (aget! u i j)
	      vy    (aget! v i j)] 
	  (if (pos? vy)
	    (.setColor g Color/RED)
	    (.setColor g Color/GREEN))
	(.drawLine g mx my (dec (+ mx (* ux scale)))
		           (dec (+ my (* vy scale)))))))))

(defn density-step [d v sources]
  (when-let [s (seq @sources)] (swap! d add-sources s))
  (swap! d diffuse-board 0 1)
);  (swap! d advection v 0))

(defn velocity-step [velocities]
  (let [[u v] @velocities]
    (reset! velocities [(diffuse-board u 1 1)
			(diffuse-board v 2 1)])
    (swap!  velocities project)
    (reset! velocities [(advection u @velocities 1)
			(advection v @velocities 2)])
    (swap!  velocities project)))

(defn dynamics [densities velocities sources panel]
  (while @running
;	 (velocity-step velocities)
	 (density-step  densities @velocities sources)
	 (.repaint panel)))
  
(defn new-source [e scale sources]
  (swap! sources conj [(int (/ (.getX e) scale)) (int (/ (.getY e) scale))]))

(defn main [scale [w h]]
  (reset! running true)
  (let [densities  (atom (new-array  w h))
	d0         (make-array Double/TYPE w h)
	velocities (atom (topdown-array [(new-array w h) (new-array w h)]))
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
    (future (dynamics densities velocities sources panel))))