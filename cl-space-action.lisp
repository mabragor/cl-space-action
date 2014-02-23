;;;; cl-space-action.lisp
(in-package #:cl-space-action)

;;; "cl-space-action" goes here. Hacks and glory await!

(defparameter *length-factor* 1)
(defparameter *thrust* 0.0005)
(defparameter *rotate* 0.00005)

(defparameter *inner-width* 40)
(defparameter *inner-height* 30)
(defparameter *out-in-ratio* 1)

(defparameter *geometry* :toric)
(defparameter *dimension* 2)
(defparameter *graviconstant* 0.0000001)

(defparameter *bg-color* (make-color :red 65535 :green 65535 :blue 65535))

(defclass material-dot ()
  ((m :initarg :mass :initform 1)
   (x :initarg :x :initform (error "ship should be positioned somewhere."))
   (y :initarg :y :initform (error "ship should be positioned somewhere."))
   (vx :initarg :velocity-x :initform 0)
   (vy :initarg :velocity-y :initform 0)
   (force :initform (list 0 0))))
   
(defclass rotator ()
  ((phi :initarg :angle :initform 0)
   (omega :initarg :rotational-velocity :initform 0)
   (epsilon :initform 0)
   (momentum-of-inertia)))

(defclass spaceship (material-dot rotator)
  ((l :initarg :length :initform 1 :reader spaceship-length)
   (w :initarg :width :initform 1 :reader spaceship-width)
   (aspect-ratio :initarg :aspect-ratio :initform 0.75 :accessor spaceship-ratio)
   (bound-points :accessor spaceship-bound-points)
   (prev-draw-data :initform nil)
   (rear-force :initform 0)
   (front-force :initform 0)
   (left-force :initform 0)
   (right-force :initform 0)
   (color :initarg :color :initform (make-color :red 0 :green 0 :blue 0))))

(defclass planet (material-dot)
  ((radius :initarg :radius :initform 1)
   (color :initarg :color :initform (make-color :red 0 :green 0 :blue 0))
   (prev-draw-data :initform nil)))

(defgeneric calculate-momentum-of-inertia (something)
  (:method ((something t))
    1))

(defmethod calculate-momentum-of-inertia ((ship spaceship))
  (with-slots (m l w aspect-ratio) ship
    (/ (* m (* l *length-factor*) (* l *length-factor*)) 12)))
  

(defmethod initialize-instance :after ((rotator rotator) &key)
  (setf (slot-value rotator 'momentum-of-inertia)
	(calculate-momentum-of-inertia  rotator)))

(defmethod initialize-instance :after ((ship spaceship) &key)
  (let ((half-length (/ (spaceship-length ship) 2))
	(half-width (/ (spaceship-width ship) 2)))
    (setf (spaceship-bound-points ship)
	  `((0 ,half-length)
	    (,half-width ,(- half-length (* (spaceship-length ship) (spaceship-ratio ship))))
	    (0 ,(- half-length))
	    (,(- half-width) ,(- half-length (* (spaceship-length ship) (spaceship-ratio ship))))))))

(defgeneric draw (window something)
  (:documentation "General draw methods"))

(defgeneric erase (window something)
  (:documentation "General erase method")
  (:method (window (something t))
    ;; just do nothing
    ))

(defmethod erase (window (ship spaceship))
  (let* ((gc (graphics-context-new window)))
    (multiple-value-bind (w h) (drawable-get-size window)
      (with-slots (prev-draw-data) ship
	(when prev-draw-data
	  (setf (graphics-context-rgb-bg-color gc) *bg-color*)
	  (setf (graphics-context-rgb-fg-color gc) *bg-color*)
	  (draw-polygon window gc nil prev-draw-data))))))

(defmethod draw (window (ship spaceship))
  (let* ((gc (graphics-context-new window)))
    (multiple-value-bind (w h) (drawable-get-size window)
      (with-slots (x y phi bound-points prev-draw-data color) ship
	;; (when prev-draw-data
	;;   (erase window ship))
	(setf prev-draw-data
	      (mapcar (lambda (pt)
			(make-point :x (round (* *out-in-ratio* (+ x (car pt))))
				    :y (round (* *out-in-ratio* (+ y (cadr pt))))))
		      (mapcar (lambda (pt)
				(list (+ (* (cos phi) (car pt)) (* (- (sin phi)) (cadr pt)))
				      (+ (* (sin phi) (car pt)) (* (cos phi) (cadr pt)))))
			      bound-points)))
	(setf (graphics-context-rgb-bg-color gc) color)
	(setf (graphics-context-rgb-fg-color gc) color)
	(draw-polygon window gc nil prev-draw-data)
	))))

(defmethod draw (window (planet planet))
  (let* ((gc (graphics-context-new window)))
    ;; (multiple-value-bind (w h) (drawable-get-size window)
    (with-slots (x y color radius prev-draw-data) planet
      (setf (graphics-context-rgb-bg-color gc) color)
      (setf (graphics-context-rgb-fg-color gc) color)
      (setf prev-draw-data (list (round (* *out-in-ratio* (- x radius)))
				 (round (* *out-in-ratio* (- y radius)))
				 (round (* (* *out-in-ratio* radius) 2))
				 (round (* (* *out-in-ratio* radius) 2))))
      (apply #'draw-arc `(,window ,gc ,t ,@prev-draw-data
				  0 ,(* 64 360))))))

(defmethod erase (window (planet planet))
  (with-slots (x y color radius prev-draw-data) planet
    (when prev-draw-data
      (let* ((gc (graphics-context-new window)))
	;; (multiple-value-bind (w h) (drawable-get-size window)

	(setf (graphics-context-rgb-bg-color gc) *bg-color*)
	(setf (graphics-context-rgb-fg-color gc) *bg-color*)
	(apply #'draw-arc `(,window ,gc ,t ,@prev-draw-data
				    0 ,(* 64 360)))))))
  

(defun prepare-window (window)
  (let* ((gc (graphics-context-new window)))
    (multiple-value-bind (w h) (drawable-get-size window)
      (setf (graphics-context-rgb-bg-color gc) *bg-color*)
      (setf (graphics-context-rgb-fg-color gc) *bg-color*)
      (draw-polygon window gc t (list (make-point :x 0 :y 0)
				      (make-point :x w :y 0)
				      (make-point :x w :y h)
				      (make-point :x 0 :y h))))))

(defgeneric move (something)
  (:documentation "General method for specification of dynamics")
  (:method ((something t))
    ;; just do nothing
    ))

(defgeneric calculate-acceleration (something)
  (:method ((something t))
    (list 0 0)))

(defmethod calculate-acceleration ((dot material-dot))
  (with-slots (force m) dot
    ;; (list 0.1 0.1)))
    ;; (format t "force: ~a~%" force)
    (list (/ (car force) m)
    	  (/ (cadr force) m))))

(defmethod calculate-acceleration :around ((ship spaceship))
  (let ((pre-accel (call-next-method)))
    (with-slots (rear-force front-force left-force right-force phi m) ship
      (let ((fx (/ (+ left-force right-force) m))
	    (fy (/ (+ front-force rear-force) m)))
	(incf (car pre-accel) (+ (* (cos phi) fx) (* (- (sin phi)) fy)))
	(incf (cadr pre-accel) (+ (* (sin phi) fx) (* (cos phi) fy)))))
    pre-accel))

(defgeneric calculate-angular-acceleration (something)
  (:method ((something t))
    0))

(defmethod calculate-angular-acceleration ((ship spaceship))
  (with-slots (left-force right-force l w aspect-ratio momentum-of-inertia) ship
    (/ (* (+ left-force right-force)
	  (- (* l *length-factor*)) aspect-ratio)
       momentum-of-inertia)))


(defmethod move :before ((dot material-dot))
  (with-slots (x y vx vy) dot
    (destructuring-bind (ax ay) (calculate-acceleration dot)
      (incf x vx)
      (incf y vy)
      (when (eq *geometry* :toric)
	(setf x (mod x *inner-width*)
	      y (mod y *inner-height*)))
      (incf vx ax)
      (incf vy ay))))

(defmethod move :before ((rotator rotator))
  (with-slots (phi omega epsilon) rotator
    (let ((e (calculate-angular-acceleration rotator)))
      (setf phi (mod (+ phi omega) (* 2 pi)))
      (incf omega e))))

(defmethod move :before ((ship spaceship))
  )

(defmacro with-muffled-bare-ref-warn (&body body)
  `(handler-bind
       ((alexandria:simple-style-warning
	 (lambda (warning)
	   (when (alexandria:starts-with-subseq
		  "bare references to struct types are deprecated."
		  (simple-condition-format-control warning))
	     (muffle-warning warning)))))
     ,@body))

(defparameter *objects* nil)

(defmethod move ((lst list))
  (dolist (elt lst)
    (move elt)))

(defmethod erase (window (lst list))
  (dolist (elt lst)
    (erase window elt)))

(defmethod draw (window (lst list))
  (dolist (elt lst)
    (draw window elt)))

(defparameter *ship1* nil)
(defparameter *ship2* nil)

(defun setup-ratios (window)
  (multiple-value-bind (w h) (drawable-get-size (widget-window window))
    (setf *out-in-ratio* (/ w *inner-width*))
    (setf *inner-height* (/ h *out-in-ratio*))))

(defgeneric interaction-force (obj1 obj2)
  (:method ((obj1 t) (obj2 t))
    (list 0 0)))

(defmethod interaction-force ((dot1 material-dot) (dot2 material-dot))
  ;; (list 0.1 0))
  (if (eq dot1 dot2)
      (list 0 0)
      (with-slots ((x1 x) (y1 y) (m1 m)) dot1
	(with-slots ((x2 x) (y2 y) (m2 m)) dot2
	  (let ((rvx (- x2 x1))
		(rvy (- y2 y1)))
	    (let ((rmod (sqrt (+ (expt rvx 2) (expt rvy 2)))))
	      (list (/ (* *graviconstant* m1 m2 rvx)
		       (expt rmod *dimension*))
		    (/ (* *graviconstant* m1 m2 rvy)
		       (expt rmod *dimension*)))))))))
		

(defun calculate-forces (lst)
  (iter (for elt in lst)
	(setf (car (slot-value elt 'force)) 0
	      (cadr (slot-value elt 'force)) 0))
  (iter (for elt1 on lst)
	(iter (for elt2 on (cdr elt1))
	      (with-slots ((force1 force)) (car elt1)
		(with-slots ((force2 force)) (car elt2)
		  (let ((force (interaction-force (car elt1) (car elt2))))
		    (incf (car force1) (car force))
		    (incf (cadr force1) (cadr force))
		    (decf (car force2) (car force))
		    (decf (cadr force2) (cadr force))))))))

(defun launch ()
  (setf *ship1* (make-instance 'spaceship :x 10 :y 10 :velocity-x 0 :velocity-y 0
			       :length 1 :width 0.5
			       :color (make-color :red 65535 :green 0 :blue 0))
	*ship2* (make-instance 'spaceship :x 30 :y 30 :velocity-x 0 :velocity-y 0
			       :rotational-velocity 0
			       :length 1 :width 0.5
			       :color (make-color :red 0 :green 0 :blue 65535)))
  (setf *objects* (list *ship1* *ship2*))
  (push (make-instance 'planet :x 20 :y 15 :radius 2 :mass 100000) *objects*)
  (within-main-loop
    (let ((window (make-instance 'gtk-window :type :toplevel :app-paintable t)))
      (connect-signal window "destroy" (lambda (widget)
					 (declare (ignore widget))
					 (leave-gtk-main)))
      (connect-signal window "expose-event"
		      (lambda (widget event)
			(declare (ignore widget event))
			(setup-ratios window)
			(prepare-window (widget-window window))))
      (connect-signal window "configure-event"
		      (lambda (widget event)
			(declare (ignore widget event))
			(setup-ratios window)
			(prepare-window (widget-window window))
			(widget-queue-draw window)))
      (connect-signal window "key-press-event"
                      (lambda (w e)
                        (declare (ignore w))
                        (ignore-errors
                          (let ((c (aref (gdk:event-key-string e) 0)))
			    (with-slots (rear-force front-force left-force right-force) *ship1*
			      (case c
				(#\w (setf rear-force  *thrust*))
				(#\s (setf front-force (- *thrust*)))
				(#\a (setf right-force *rotate*))
				(#\d (setf left-force (- *rotate*)))))
			    (with-slots (rear-force front-force left-force right-force) *ship2*
			      (case c
				(#\i (setf rear-force  *thrust*))
				(#\k (setf front-force (- *thrust*)))
				(#\j (setf right-force *rotate*))
				(#\l (setf left-force (- *rotate*)))))))
			nil))
      (connect-signal window "key-release-event"
                      (lambda (w e)
                        (declare (ignore w))
                        (ignore-errors
                          (let ((c (aref (gdk:event-key-string e) 0)))
			    (with-slots (rear-force front-force left-force right-force) *ship1*
			      (case c
				(#\w (setf rear-force 0))
				(#\s (setf front-force 0))
				(#\a (setf right-force 0))
				(#\d (setf left-force 0))))
			    (with-slots (rear-force front-force left-force right-force) *ship2*
			      (case c
				(#\i (setf rear-force  0))
				(#\k (setf front-force 0))
				(#\j (setf right-force 0))
				(#\l (setf left-force 0))))))
			nil))
      (gtk-main-add-timeout 10 (lambda ()
				 (with-muffled-bare-ref-warn
				   (calculate-forces *objects*)
				   (move *objects*)
				   (erase (widget-window window) *objects*)
				   (draw (widget-window window) *objects*)
				   t))
			    :priority glib:+g-priority-high-idle+)
      (widget-show window)
      (push :pointer-motion-mask (gdk-window-events (widget-window window))))))
