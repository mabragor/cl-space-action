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

(defclass material-dot ()
  ((m :initarg :mass :initform 1)
   (x :initarg :x :initform (error "ship should be positioned somewhere."))
   (y :initarg :y :initform (error "ship should be positioned somewhere."))
   (vx :initarg :velocity-x :initform 0)
   (vy :initarg :velocity-y :initform 0)))
   
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
   (right-force :initform 0)))

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
  (:documentation "General erase method"))

(defmethod erase (window (ship spaceship))
  (let* ((gc (graphics-context-new window)))
    (multiple-value-bind (w h) (drawable-get-size window)
      (with-slots (prev-draw-data) ship
	(when prev-draw-data
	  (setf (graphics-context-rgb-bg-color gc) (make-color :red 65535 :green 65535 :blue 65535))
	  (setf (graphics-context-rgb-fg-color gc) (make-color :red 65535 :green 65535 :blue 65535))
	  (draw-polygon window gc nil prev-draw-data))))))

(defmethod draw (window (ship spaceship))
  (let* ((gc (graphics-context-new window)))
    (multiple-value-bind (w h) (drawable-get-size window)
      (with-slots (x y phi bound-points prev-draw-data) ship
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
	(setf (graphics-context-rgb-bg-color gc) (make-color :red 65535 :green 0 :blue 0))
	(setf (graphics-context-rgb-fg-color gc) (make-color :red 65535 :green 0 :blue 0))
	(draw-polygon window gc nil prev-draw-data)
	))))

(defun prepare-window (window)
  (let* ((gc (graphics-context-new window)))
    (multiple-value-bind (w h) (drawable-get-size window)
      (setf (graphics-context-rgb-bg-color gc) (make-color :red 65535 :green 65535 :blue 65535))
      (setf (graphics-context-rgb-fg-color gc) (make-color :red 65535 :green 65535 :blue 65535))
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

(defmethod calculate-acceleration ((ship spaceship))
  (with-slots (rear-force front-force left-force right-force phi m) ship
    (let ((fx (/ (+ left-force right-force) m))
	  (fy (/ (+ front-force rear-force) m)))
      (list (+ (* (cos phi) fx) (* (- (sin phi)) fy))
	    (+ (* (sin phi) fx) (* (cos phi) fy))))))

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


(defun launch ()
  (setf *ship1* (make-instance 'spaceship :x 10 :y 10 :velocity-x 0 :velocity-y 0
			       :length 1 :width 0.5)
	*ship2* (make-instance 'spaceship :x 30 :y 30 :velocity-x 0 :velocity-y 0
			       :rotational-velocity 0
			       :length 1 :width 0.5))
  (setf *objects* (list *ship1* *ship2*))
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
				    (move *objects*)
				    (erase (widget-window window) *objects*)
				    (draw (widget-window window) *objects*)
				    t))
			    :priority glib:+g-priority-high-idle+)
      (widget-show window)
      (push :pointer-motion-mask (gdk-window-events (widget-window window))))))
