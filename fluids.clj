(import '(java.awt Color Graphics RenderingHints Graphics2D)
        '(java.awt.image BufferedImage)
        '(java.awt.event MouseListener MouseMotionListener)
        '(javax.swing JFrame JPanel))

;; Globals

(def *dt* (double 1.0))
(def running         (atom true))
(def show-grid       (atom true))

;; Helpers

(defmacro defworker [name args & body]
  " Defines a worker which discontinues looping his main body
    if the global atom 'running' is non-true. Exceptions are
    caught and printed "
  `(defn ~name ~args
     (while @running
	    (try ~@body (catch Exception e#
			  (-> e# .getMessage println))))))

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

(defn up-array [orig]
  (let [[u v] orig
	[w h] [(count v) (-> v first count)]]
    (do-board [w h]
	      (aset! u i j 0)
	      (aset! v i j -0.005))
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

;; Sources

(defn add-sources
  [board sources]
  (doseq [[x y] sources] (aset! board x y 500.0))
  board)

;; Diffusion

(defmacro cswap! [arr x y old p]
  `(aset! ~arr ~x ~y (if ~p (- ~old) ~old)))

(defn set-bounds [arr b]
  (let [N      (int (- (count arr) (int 2)))
	n1     (unchecked-inc N)
	z      (int 0)]
    (loop [i (int 1)]
      (when (<= i N)
	(cswap! arr z  i (aget! arr 1 i) (= b 1))
	(cswap! arr n1 i (aget! arr N i) (= b 1))
	(cswap! arr i  z (aget! arr i 1) (= b 2))
	(cswap! arr i n1 (aget! arr i N) (= b 2))
	(recur (unchecked-inc i))))
    (aset! arr z z   (* 0.5 (+ (aget! arr 1 z)  (aget! arr z 1))))
    (aset! arr z n1  (* 0.5 (+ (aget! arr 1 n1) (aget! arr z N))))
    (aset! arr n1 z  (* 0.5 (+ (aget! arr N z)  (aget! arr n1 1))))
    (aset! arr n1 n1 (* 0.5 (+ (aget! arr N n1) (aget! arr n1 N))))
    arr))

(defmacro map! [f lookup]
  `(let [[w# h#] [(count ~lookup) (-> ~lookup first count)]]
     (let [board# (make-array Double/TYPE h# w#)]
       (do-board [w# h#]
		 (let [~'above  (double (aget! ~lookup (unchecked-dec ~'i) ~'j))
		       ~'below  (double (aget! ~lookup (unchecked-inc ~'i) ~'j))
		       ~'left   (double (aget! ~lookup ~'i (unchecked-dec ~'j)))
		       ~'right  (double (aget! ~lookup ~'i (unchecked-inc ~'j)))
		       ~'self   (double (aget! ~lookup ~'i ~'j))]
		   (aset! board# ~'i ~'j ~f)))
	 (set-bounds board# 0))))

(defn diffuse-board
  [board b diffusion dt]
  (let [sz         (count board)
	dr         (double (* dt diffusion sz sz))
	dconst     (double (* 4 dr))]
    (map! (/ (+ self (* dr (+ (+ above below) (+ left right))))
	     dconst) board)))

(defn advection [densities velocities b]
  (let [sz    (count densities)
	sz1   (- sz (int 2))
	retr  (new-array sz sz)
	half  (double 0.5)
	[u v] velocities]
    (do-board [sz sz]
      (let [x  (- (int i) (* sz1 (double (aget! u i j))))
	    y  (- (int j) (* sz1 (double (aget! v i j))))
	    x  (cond (< x half)        half
		     (> x (+ half sz1)) (unchecked-dec sz1)
		     :else (double x))
	    y  (cond (< y half)        half
		     (> y (+ half sz1)) (unchecked-dec sz1)
		     :else (double y))
	    i0  (int x)
	    i1  (unchecked-inc i0)
	    j0  (int y)
	    j1  (unchecked-inc j0)
	    s1  (double (- x i0))
	    s0  (double (- (int 1) s1))
	    t1  (double (- y j0))
	    t0  (double (- (int 1) t1))]
	(aset! retr i j
	       (+ (* s0 (+ (* t0 (double (aget! densities i0 j0)))
			   (* t1 (double (aget! densities i0 j1)))))
		  (* s1 (+ (* t0 (double (aget! densities i1 j0)))
			   (* t1 (double (aget! densities i1 j1)))))))))
    (set-bounds retr b)))

(defn project [velocities]
  (let [[u v]    velocities
	[w h]    [(count u) (-> u first count)]
	r        (double (/ 1.0 h))
	p        (new-array w h)
	div      (new-array w h)
	half     (double 0.5)]
    (do-board [w h]
       (aset! div i j
	      (* (double -0.5) r
		 (+ (- (double (aget! u (unchecked-inc i) j))
		       (double (aget! u (unchecked-dec i) j)))
		    (- (double (aget! v i (unchecked-inc j)))
		       (double (aget! v i (unchecked-dec j))))))))
    (let [div (set-bounds div 0)
	  p   (set-bounds p   0)]
      (do-board [w h]
	 (aset! p i j
		(/  (+ (+  (double (aget! div i j))
			   (double (aget! p (unchecked-dec i) j)))
		       (+  (double (aget! p (unchecked-inc i) j))
			   (double (aget! p i (unchecked-dec j))))
		       (double (aget! p i (unchecked-inc j))))
		   (double 4))))
      (let [p (set-bounds p 0)]
	(do-board [w h]
		  (aset! u i j
			 (-
			    (double (aget! u i j))
			    (/  (* half
				   (- (double (aget! p (unchecked-inc i) j))
				      (double (aget! p (unchecked-dec i) j))))
				r)))
		  (aset! v i j
			 (-  (double (aget! v i j))
			     (/  (* half
				    (- (double (aget! p i (unchecked-inc j)))
				       (double (aget! p i (unchecked-dec j)))))
				 r))))
	[(set-bounds u 1) (set-bounds v 2)]))))

;; Rendering

(defn render-scene [#^Graphics2D g w h scale board velocities]
  (doto g
    (.setColor Color/BLACK)
    (.fillRect 0 0 (* scale w) (* scale h)))
  (do-board [w h]
    (let [density (min 255 (int (aget! board i j)))]
      (.setColor g (Color. density density density))
      (.fillRect g (* i scale) (* j scale) scale scale)
      (when @show-grid
	(let [[u v] velocities
	      mx    (+ (* i scale) (/ scale 2)) ; Middle of cell, X
	      my    (+ (* j scale) (/ scale 2))
	      ux    (aget! u i j)
	      vy    (aget! v i j)]
	  (if (pos? vy)
	    (.setColor g Color/RED)
	    (.setColor g Color/GREEN))
	(.drawLine g mx my (+ mx (* ux scale))
		           (+ my (* vy scale))))))))

(defworker render [panel fps]
  (.repaint panel)
  (Thread/sleep (* 1000 (/ 1 fps))))

;; Dynamics

(defn density-step [d v sources]
  (when-let [s (seq @sources)]
   (swap! d add-sources s))
  (swap! d diffuse-board 0 1 1)
  (swap! d advection v 0))

(defn velocity-step [velocities]
  (let [drefv @velocities
	[u v] drefv]
    (reset! velocities [(diffuse-board u 1 1 1)	(diffuse-board v 2 1 1)])
    (swap!  velocities project)
    (reset! velocities [(advection u drefv 1)	(advection v drefv 2)])
    (swap!  velocities project)))

(defworker dynamics [densities velocities sources panel]
  (velocity-step velocities)
  (density-step  densities @velocities sources))

;; UI + UI Utilities

(defn new-source [e scale sources]
  (swap! sources conj [(int (/ (.getX e) scale)) (int (/ (.getY e) scale))]))

(defn add-density [board x y d]
  (let [w   (count board)]
    (aset! board (int x) (int y) (double d))
    board))

(defn add-force [board x y u v]
  (let [[horiz vert] board
	w            (count horiz)]
    (aset! horiz (int x) (int y) (double u))
    (aset! vert  (int x) (int y) (double v))
    [horiz vert]))

(defn mouse-click [sources scale]
  (proxy [java.awt.event.MouseListener] []
    (mouseExited     [e] nil)
    (mouseEntered    [e] nil)
    (mouseReleased   [e] nil)
    (mousePressed    [e] nil)
    (mouseClicked    [e]
	 (if (= java.awt.event.MouseEvent/BUTTON1 (.getButton e))
	   (new-source e scale sources)
	   (swap! show-grid not)))))

(defn mouse-move [mouse densities velocities scale sz]
  (proxy [java.awt.event.MouseMotionListener] []
    (mouseMoved   [e] (reset! mouse [(/ (.getX e) scale) (/ (.getY e) scale)]))
    (mouseDragged [e]
	   (let [[mx   my]  [(/ (.getX e) scale) (/ (.getY e) scale)]
		 [omx omy]  @mouse
		 dx     (- mx omx)
		 dy     (- my omy)
		 len    (max 1 (+ (Math/sqrt (+ (* dx dx) (* dy dy))) 0.5))]
	     (dotimes [i len]
	       (let [cx   (* scale (/ (+ omx (* (/ i len) dx)) (* sz scale)))
		     cy   (* scale (/ (+ omy (* (/ i len) dy)) (* sz scale)))]
		 (swap!  densities  add-density mx my 1000.0)
		 (swap!  velocities add-force   mx my dx dy)
		 (reset! mouse      [mx my])))))))

(defn main [scale [w h]]
  (reset! running   true)
  (reset! show-grid false)
  (let [densities  (atom (new-array w h))
	velocities (atom (up-array [(new-array w h) (new-array w h)]))
	sources    (atom [])
	mouse      (atom [0 0])
	panel      (doto (proxy [JPanel] []
			   (paint [g] (render-scene g w h scale
						    @densities @velocities))))]
    (doto (JFrame. "Fluid Dynamics")
      (.addWindowListener      (proxy [java.awt.event.WindowAdapter] []
				 (windowClosing [_] (reset! running false))))
      (.addMouseListener       (mouse-click sources scale))
      (.addMouseMotionListener (mouse-move  mouse densities velocities scale w))
      (.setSize                (* scale w) (* scale  h))
      .pack .show
      (.add panel))
    (future (render panel 20))
    (future (dynamics densities velocities sources panel))))

(main 7 [80 80])