;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage :sugiyama
  (:use :cl :iterate :alexandria :qt :hemlock-interface)
  (:export #:test))

(in-package :sugiyama)
(named-readtables:in-readtable :qt-hemlock)


;;;;
;;;; Layout Computation
;;;;

(iterate::defclause-driver (for sym in-graph table)
    "Nodes of a graph"
  (iterate::top-level-check)
  (unless (symbolp sym)
    (iterate::clause-error "~a should be a symbol" sym))
  (let* ((iterator (gensym "HASH-TABLE-ITERATOR-"))
         (more?    (gensym))
         (var-spec `(values ,more? nil ,sym))
         (setqs    (iterate::do-dsetq var-spec `(,iterator)))
         (test     `(if (not ,more?) (go ,iterate::*loop-end*))))
    ;; FIXME 2004-11-11 destructure only after termination test
    (setq iterate::*loop-end-used?* t)
    (iterate::add-loop-body-wrapper
     `(with-hash-table-iterator (,iterator (graph-nodes ,table))))
    (iterate::return-driver-code :next (list setqs test)
                                 :variable var-spec)))
#+(or)
(iterate::defclause-driver (for var in-graph table)
  "Elements of a list"
  (iterate::top-level-check)
  (let* ((on-var (iterate::make-var-and-default-binding 'list :type 'list))
         (setqs (iterate::do-dsetq var `(car ,on-var)))
         (test `(if (endp ,on-var) (go ,iterate::*loop-end*))))
    (setq iterate::*loop-end-used?* t)
    (iterate::return-driver-code
     :initial `((setq ,on-var (sorted-nodes ,table)))
     :next (list test
                 setqs
                 (iterate::generate-function-step-code on-var ''cdr))
     :variable var)))

(defmacro collecting (&body body)
  `(iter (repeat 1) ,@body))

(defclass graph ()
  ((nodes :accessor graph-nodes)
   (node-class :initform 'node
               :initarg :node-class
               :accessor graph-node-class)
   (edge-class :initform 'edge
               :initarg :edge-class
               :accessor graph-edge-class)))

(defclass edge ()
  ((from :initarg :from
         :accessor edge-from)
   (to :initarg :to
       :accessor edge-to)))

(defclass node ()
  ((graph :initarg :graph
          :accessor node-graph)
   (key :initarg :key
        :accessor node-key)
   (+ :initform nil
      :accessor node+)
   (- :initform nil
      :accessor node-)))

(defclass fancy-node (node)
  ((label :initarg :label
          :initform nil
          :accessor node-label)
   (color :initarg :color
          :initform nil
          :accessor node-color)))

(defclass node-with-position (fancy-node)
  ((layer :initform nil
          :initarg :layer
          :accessor node-layer)
   (x :initarg :x
      :initform nil
      :accessor node-x)
   (dummyp :initform nil
           :initarg :dummyp
           :accessor dummy-node-p)
   (width :initarg :width
          :accessor node-width)
   (height :initarg :height
           :accessor node-height)
   (graphics-items :initarg :graphics-items
                   :initform nil
                   :accessor node-graphics-items)))

(defclass sugiyama-node (node-with-position)
  ((self-cycle-p :initform nil
                 :accessor node-self-cycle-p)
   (%pi :initform nil
        :initarg :pi
        :accessor node-pi)
   (dummy-source :initform nil
                 :initarg :dummy-source
                 :accessor dummy-node-source)
   (dummy-target :initform nil
                 :initarg :dummy-target
                 :accessor dummy-node-target)
   (original+ :accessor original-node+)))

(defclass static-node (node-with-position)
  ((y :initarg :y
      :accessor y)
   (original-node :initarg :original-node
                  :accessor node-original-node)))

(defclass sugiyama-edge (edge)
  ((flip-type :initarg :flip-type
              :initform nil
              :accessor edge-flip-type)
   (graphics-item :initarg :graphics-item
                  :initform nil
                  :accessor edge-graphics-item)))

(defmethod initialize-instance :after ((graph graph) &key (test 'eql))
  (setf (graph-nodes graph) (make-hash-table :test test)))

(defmethod print-object ((g graph) stream)
  (print-unreadable-object (g stream :type t :identity t)
    (format stream "with ~D nodes, ~D edges"
            (hash-table-count (graph-nodes g))
            (iter (for v in-graph g)
                  (sum (+ (length (node+ v)) (length (node- v))))))))

(defmethod print-object ((v node) stream)
  (print-unreadable-object (v stream :type t)
    (format stream "~A, ~D in, ~D out"
            (node-key v)
            (length (node- v))
            (length (node+ v)))))

(defmethod print-object ((e edge) stream)
  (print-unreadable-object (e stream :type t)
    (format stream "from ~A to ~A"
            (node-key (edge-from e))
            (node-key (edge-to e)))))

(defun find-node (key g &optional (errorp t))
  (or (gethash key (graph-nodes g))
      (when errorp
        (error "node not found: ~A" key))))

(defun intern-node (key g &rest initargs)
  (or (find-node key g nil)
      (setf (gethash key (graph-nodes g))
            (apply #'make-instance
                   (graph-node-class g)
                   :key key
                   :graph g
                   initargs))))

(defun map-nodes (fn g)
  (iter (for v in-graph g)
        (funcall fn v)))

(defun list-nodes (g)
  (iter (for v in-graph g)
        (collect v)))

(defun sorted-nodes (g)
  (sort (list-nodes g) #'key<))

(defun key< (a b)
  (when (symbolp a) (setf a (symbol-name a)))
  (when (symbolp b) (setf b (symbol-name b)))
  (cond
    ((and (stringp a) (stringp b))
     (string-lessp a b))
    ((stringp a)
     t)
    ((stringp b)
     nil)
    (t
     (key< (second a) (second b)))))

(defun list-edges (g)
  (iter (for v in-graph g)
        (appending (node+ v))))

(defun add-edge (from to &rest initargs)
  (when (find to (node+ from) :key #'edge-to)
    (error "duplicate edge"))
  (let* ((g (node-graph from))
         (edge (apply #'make-instance
                      (graph-edge-class g)
                      :from from
                      :to to
                      initargs)))
    (push edge (node+ from))
    (push edge (node- to))
    edge))

(defun delete-edge (e)
  (with-slots (from to) e
    (removef (node+ from) e)
    (removef (node- to) e)
    (setf (edge-from e) nil)
    (setf (edge-to e) nil)))

(defun find-edge (v w)
  (find w (node+ v) :key #'edge-to))

(defun delete-edge* (from to)
  (delete-edge (or (find-edge from to)
                   (error "no edge from ~A to ~A" from to))))

(defun delete-node (v)
  (mapc #'delete-edge (node- v))
  (mapc #'delete-edge (node+ v))
  (remhash (node-key v) (graph-nodes (node-graph v)))
  (setf (node-graph v) nil))

(defun graph-test (g)
  (hash-table-test (graph-nodes g)))

(defun make-static-graph (g)
  (let ((h (make-instance 'graph
                          :test (graph-test g)
                          :node-class 'static-node
                          :edge-class 'sugiyama-edge)))
    (iter (for node in-graph g)
          (intern-node (node-key node)
                       h
                       :dummyp (dummy-node-p node)
                       :original-node (if (typep node 'static-node)
                                          (node-original-node node)
                                          node)
                       :width (node-width node)
                       :height (node-height node)
                       :x (node-x node)
                       :layer (node-layer node)
                       :y (y node)
                       :label (node-label node)
                       :color (node-color node)))
    (iter (for node in-graph g)
          (dolist (e (node+ node))
            (add-edge (find-node (node-key (edge-from e)) h)
                      (find-node (node-key (edge-to e)) h))))
    h))

(defun max-layer-width (g)
  (let ((widths (make-array (count-layers g) )))
    (iter (for v in-graph g)
          (incf (elt widths (node-layer v))))
    (reduce #'max widths)))

(defun checkpoint-graph-edges (g)
  (iter (for v in-graph g)
        (checkpoint-node-edges v)))

(defun checkpoint-node-edges (v)
  (setf (original-node+ v) (mapcar #'clone-edge (node+ v)))
  #+nil (setf (original-node- v) (mapcar #'clone-edge (node- v))))

(defun clone-edge (e)
  (make-instance 'edge
                 :from (edge-from e)
                 :to (edge-to e)))

(defun sugiyama (g size-function &key collapse)
  (checkpoint-graph-edges g)
  (delete-self-cycles g)
  ;; (flip-edges/naive-greedy g)
  (flip-edges/sink-and-source-aware-greedy g)
  ;; (compute-layers/minimal-height g)
  (compute-layers/fixed-width g (ceiling (sqrt (hash-table-count (graph-nodes g)))))
  (add-dummy-nodes g)
  (iter (for v in-graph g)
        (setf (values (node-width v) (node-height v))
              (if (dummy-node-p v)
                  (values 10 0)
                  (funcall size-function (or (node-label v)
                                             (node-key v))))))
  (let ((layers (make-layer-lists g)))
    (minimize-crossing/barycenter layers)
    #+nil (minimize-crossing/greedy-switch3 layers)
    (minimize-crossing/greedy-switch layers)
    (ecase collapse
      ((nil))
      (:gently (collapse-dummy-nodes layers))
      (:aggressively (collapse-dummy-nodes/aggressively layers)))
    (compute-x-coordinates g layers)
    (foo layers)))

(defun unsugiyama (g)
  (let* ((all-nodes (list-nodes g))
         (dummy-nodes (remove-if-not #'dummy-node-p all-nodes))
         (proper-nodes (set-difference all-nodes dummy-nodes)))
    ;; delete dummy nodes
    (mapc #'delete-node dummy-nodes)
    ;; delete all edges
    (dolist (v proper-nodes)
      (mapc #'delete-edge (node+ v)))
    ;; reset node subclass slots and re-add edges
    (dolist (v proper-nodes)
      (let ((+ (original-node+ v)))
        (change-class v 'fancy-node)
        (change-class v (graph-node-class g))
        (dolist (e +)
          (add-edge (edge-from e) (edge-to e)))
        (setf (original-node+ v) +)))))

(defun sources (g)
  (iter (for v in-graph g)
        (unless (node- v)
          (collect v))))

(defun sinks (g)
  (iter (for v in-graph g)
        (unless (node+ v)
          (collect v))))

(defun prune (start)
  (let ((seen (make-hash-table)))
    (labels ((recurse (v)
               (unless (gethash v seen)
                 (setf (gethash v seen) t)
                 (mapc (compose #'recurse #'edge-to) (node+ v)))))
      (recurse start))
    (iter (for v in-graph (node-graph start))
          (unless (gethash v seen)
            (delete-node v)))))

(defun delete-self-cycles (g)
  (map-nodes (lambda (v)
               (when-let ((e (find v (node+ v) :key #'edge-to)))
                 (setf (node-self-cycle-p v) t)
                 (delete-edge e)))
             g))

(defun any-node (g)
  (block nil
    (map-nodes (lambda (v) (return v)) g)))

(defun dfo (fn g)
  (let ((seen (make-hash-table))
        (cycles? nil))
    (labels ((recurse (v)
               (ecase (gethash v seen :white)
                 (:black)
                 (:gray
                  (setf cycles? t))
                 (:white
                  (setf (gethash v seen) :gray)
                  (funcall fn v)
                  (mapc (compose #'recurse #'edge-to) (node+ v))
                  (setf (gethash v seen) :black)))))
      (iter (for v in-graph g)
            (recurse v)))
    cycles?))

(defun cycles? (g)
  (dfo (constantly nil) g))

(defun flip-edge (e)
  (let ((f (find-edge (edge-to e) (edge-from e))))
    (cond
      (f
       (assert (eq (edge-flip-type f) :unknown))
       (setf (edge-flip-type f) :existing))
      (t
       (add-edge (edge-to e) (edge-from e) :flip-type :extra)))
    (delete-edge e)))

(defun flip-edges/naive-greedy (g)
  (dolist (e (list-edges g))
    (setf (edge-flip-type e) :unknown))
  (map-nodes (lambda (v)
               (mapc #'flip-edge (if (> (length (node+ v)) (length (node- v)))
                                     (node- v)
                                     (node+ v))))
             g)
  (dolist (e (list-edges g))
    (when (eq (edge-flip-type e) :unknown)
      (setf (edge-flip-type e) nil))))

(defun flip-edges/sink-and-source-aware-greedy (g)
  (let ((nodes (make-hash-table))
        (edges (make-hash-table)))
    (iter (for v :in-graph g)
          (setf (gethash v nodes) t))
    (iter (for (v nil) :in-hashtable nodes)
          (dolist (edge (node+ v))
            (setf (gethash edge edges) t)
            (setf (edge-flip-type edge) :unknown)))
    (labels ((%node+ (v)
               (remove-if-not #'edge-alive-p (node+ v)))
             (%node- (v)
               (remove-if-not #'edge-alive-p (node- v)))
             (edge-alive-p (e)
               (gethash e edges))
             (find-source ()
               (iter (for (v nil) :in-hashtable nodes)
                     (when (and (%node+ v) (null (%node- v)))
                       (return v))))
             (find-sink ()
               (iter (for (v nil) :in-hashtable nodes)
                     (when (and (%node- v) (null (%node+ v)))
                       (return v))))
             (maximize-degree-diff ()
               (iter (for (v nil) :in-hashtable nodes)
                     (let ((diff (- (length (%node+ v))
                                    (length (%node- v)))))
                       (finding v maximizing diff))))
             (mark-edges (edges-to-mark)
               (dolist (e edges-to-mark)
                 (remhash e edges)
                 (ecase (edge-flip-type e)
                   (:existing)
                   (:unknown (setf (edge-flip-type e) nil)))))
             (discard-node (v)
               (dolist (e (node+ v)) (remhash e edges))
               (dolist (e (node- v)) (remhash e edges))
               (remhash v nodes)))
      (iter (while (plusp (hash-table-count nodes)))
            ;; process sinks, marking their edges as good
            (iter (for sink = (find-sink))
                  (while sink)
                  (mark-edges (%node- sink))
                  (discard-node sink))
            ;; delete isolated nodes
            (iter (for (v nil) :in-hashtable nodes)
                  (when (and (null (%node- v)) (null (%node+ v)))
                    (discard-node v)))
            ;; process sources, marking their edges as good
            (iter (for source = (find-source))
                  (while source)
                  (mark-edges (%node+ source))
                  (discard-node source))
            (let ((v (maximize-degree-diff)))
              (when v
                (mapc #'flip-edge (%node- v))
                (mark-edges (%node+ v))
                (discard-node v)))))))

(defun dfo2 (fn g)
  (let ((seen (make-hash-table))
        (cycles? nil))
    (labels ((recurse (v)
               (ecase (gethash v seen :white)
                 (:black)
                 (:gray
                  (setf cycles? t))
                 (:white
                  (setf (gethash v seen) :gray)
                  (mapc (compose #'recurse #'edge-to) (node+ v))
                  (setf (gethash v seen) :black)
                  (funcall fn v)))))
      (iter (for v in-graph g)
            (recurse v)))
    cycles?))

(defun toposort (g)
  (let* ((nodes '())
         (cycles? (dfo2 (lambda (v) (push v nodes)) g)))
    (assert (not cycles?))
    nodes))

(defun compute-layers/minimal-height (g)
  (let ((nodes (reverse (toposort g))))
    (dolist (v nodes)
      (setf (node-layer v)
            (1+ (or (reduce #'max
                            (node+ v)
                            :key (compose #'node-layer #'edge-to)
                            :initial-value 0)
                    0))))
    (let ((n (count-layers g)))
      (iter (for w in-graph g)
            (setf (node-layer w) (abs (- (node-layer w) n)))))))

(defun predecessor-pis (v)
  (sort (remove-duplicates (mapcar (compose #'node-pi #'edge-from)
                                   (node- v)))
        #'>))

(defun list-< (a b)
  (if (null a)
      (null b)
      (and b
           (or (< (car a) (car b))
               (and (eql (car a) (car b))
                    (list-< (cdr a) (cdr b)))))))

(defun predecessor-pi-min (a b)
  (if (list-< (predecessor-pis a)
              (predecessor-pis b))
      a
      b))

(defun compute-layers/fixed-width (g width)
  (let ((n (hash-table-count (graph-nodes g))))
    (iter (for v in-graph g)
          (setf (node-pi v) (1+ n))
          (setf (node-layer v) nil))
    (let ((nodes (make-hash-table)))
      (iter (for v in-graph g) (setf (gethash v nodes) t))
      (dotimes (i n)
        (let ((v (reduce #'predecessor-pi-min
                         (iter (for (v nil) in-hashtable nodes) (collect v)))))
          (remhash v nodes)
          (setf (node-pi v) i))))
    (let ((k 0)
          (nodes (make-hash-table))
          (widths (make-array 1
                              :fill-pointer 1
                              :initial-element 0
                              :adjustable t)))
      (iter (for v in-graph g) (setf (gethash v nodes) t))
      (dotimes (i n)
        (let ((u (find-if
                  (lambda (u)
                    (iter (for edge in (node+ u))
                          (always (node-layer (edge-to edge)))))
                  (sort (iter (for (u nil) in-hashtable nodes) (collect u))
                        #'>
                        :key #'node-pi))))
          (remhash u nodes)
          (unless (and (< (elt widths k) width)
                       (iter (for edge in (node+ u))
                             (always (< (node-layer (edge-to edge)) k))))
            (incf k)
            (vector-push-extend 0 widths))
          (setf (node-layer u) k)
          (incf (elt widths k))))
      ;; the above algorithm sometimes puts nodes deeper than needed.
      ;; let's try to move them up a little sometimes.
      (iter
       (while (let ((changedp nil))
                (iter (for w in-graph g)
                      (let* ((current-layer (node-layer w))
                             (a (and (node- w)
                                     (1- (reduce #'min
                                                 (from-nodes w)
                                                 :key #'node-layer))))
                             (b (and (node+ w)
                                     (1+ (reduce #'max
                                                 (to-nodes w)
                                                 :key #'node-layer))))
                             (ideal-layer (if a
                                              (if b (round (+ a b) 2) a)
                                              b)))
                        (when (and ideal-layer (> ideal-layer current-layer))
                          (iter (for i from ideal-layer
                                     downto (1+ current-layer))
                                (when (< (elt widths i) width)
                                  (setf (node-layer w) i)
                                  (decf (elt widths current-layer))
                                  (incf (elt widths i))
                                  (setf changedp t)
                                  (return))))))
                changedp)))))
  (let ((n (count-layers g)))
    (iter (for w in-graph g)
          (setf (node-layer w) (- n (node-layer w) 1)))))

(defun count-layers (g)
  (1+ (iter (for w in-graph g)
            (maximizing (node-layer w)))))

(defun make-layer-lists (g)
  (let* ((n (count-layers g))
         (layers (make-array n)))
    (dotimes (i n)
      (setf (elt layers i) (list :head)))
    (iter (for w in-graph g)
          (push w (cdr (elt layers (node-layer w))))
          (setf (node-x w) (length (elt layers (node-layer w)))))
    (remove-if (lambda (x) (null (cdr x)))
               layers)))

(defun add-dummy-nodes (g)
  (dolist (v (list-nodes g)) ;avoid mapping over hash table being mutated
    (dolist (e (node+ v))
      (let ((cursor v)
            (w (edge-to e)))
        (iter
         (for i from (1+ (node-layer v)) below (node-layer w))
         (for j from 0)
         (let ((dummy (intern-node (list :dummy (node-key v) (node-key w) j)
                                   g
                                   :layer i
                                   :dummyp t
                                   :dummy-source v
                                   :dummy-target w)))
           (add-edge cursor dummy)
           (setf cursor dummy)))
        (unless (eq cursor v)
          (add-edge cursor w)
          (delete-edge e))))))

(defun to-nodes (v)
  (mapcar #'edge-to (node+ v)))

(defun from-nodes (v)
  (mapcar #'edge-from (node- v)))

(defun minimize-crossing/barycenter (layers)
  ;; Sweep forward:
  (iter (for i from 1 to (1- (length layers)))
        (crossing-sweep-step/barycenter (elt layers i) #'from-nodes))
  ;; And back:
  (iter (for i from (- (length layers) 2) downto 0)
        (crossing-sweep-step/barycenter (elt layers i) #'to-nodes))
  ;; re-sort the layer lists, now that node order has been established:
  (iter (for layer in-vector layers)
        (setf (cdr layer) (sort (copy-list (cdr layer)) #'< :key #'node-x))))

(defun crossing-sweep-step/barycenter (layer direction)
  (declare (ignore direction))          ;?!
  (dolist (v (cdr layer))
    (when-let ((reference-nodes (append (from-nodes v) (to-nodes v))))
      (setf (node-x v)
            (- (round (/ (iter (for w in reference-nodes)
                               (sum (+ (node-x w)
                                       (/ (node-width w) 2))))
                         (length reference-nodes)))
               (node-width v)))))
  (iter (for (v w) on (sort (copy-list (cdr layer)) #'< :key #'node-x))
        (when (and w (>= (node-x v) (node-x w)))
          (setf (node-x w) (+ (node-x v) 1)))))

(defun minimize-crossing/greedy-switch (layers)
  (iter (for i from 1 to (1- (length layers)))
        (crossing-sweep-step/greedy-switch (elt layers i))))

(defun crossing-number (u v)
  (iter (for e in (node- u))
        (sum (iter (for f in (node- v))
                   (counting (> (node-x (edge-from e))
                                (node-x (edge-from f))))))))

(defun crossing-sweep-step/greedy-switch (layer)
  (iter
   (while
       (let ((changedp nil))
         (iter (for cons on (cdr layer))
               (for (u v) = cons)
               (when (and v (> (crossing-number u v) (crossing-number v u)))
                 (rotatef (first cons) (second cons))
                 (rotatef (node-x u) (node-x v))
                 (setf changedp t)))
         changedp))))

(defun minimize-crossing/greedy-switch3 (layers)
  (iter (for i from 1 to (1- (length layers)))
        (crossing-sweep-step/greedy-switch3 (elt layers i))))

(defun crossing-sweep-step/greedy-switch3 (layer)
  (iter
   (while
       (let ((changedp nil))
         (iter (for cons on (cdr layer))
               (for (u v w) = cons)
               (when (and w (> (+ (crossing-number u v) (crossing-number v w))
                               (+ (crossing-number w v) (crossing-number v u))))
                 (rotatef (first cons) (third cons))
                 (rotatef (node-x u) (node-x w))
                 (setf changedp t)))
         changedp))))

(defun collapse-dummy-nodes (layers)
  (iter (for layer in-vector layers)
        (let* ((nodes (cdr layer))
               (v (car nodes)))
          (setf (cdr layer)
                (cons v
                      (iter (for w in (cdr nodes))
                            (cond
                              ((and (node-graph v)
                                    w
                                    (dummy-node-p v)
                                    (dummy-node-p w)
                                    (null
                                     (cdr
                                      (remove-duplicates
                                       (append (node- v)
                                               (node+ v)
                                               (node- w)
                                               (node+ w))
                                       :key #'edge-flip-type)))
                                    (eq (dummy-node-target v)
                                        (dummy-node-target w)))
                               (dolist (e (node- w))
                                 (unless (find-edge (edge-from e) v)
                                   (add-edge (edge-from e) v)))
                               (dolist (e (node+ w))
                                 (unless (find-edge v (edge-to e))
                                   (add-edge v (edge-to e))))
                               (delete-node w))
                              (t
                               (collect w)
                               (setf v w))))))))
  ;; dummy-node-source is now invalid.  while sweeping backward,
  ;; fix it up, now as a list.
  (iter (for layer in-vector layers)
        (let ((nodes (cdr layer)))
          (dolist (v nodes)
            (when (dummy-node-p v)
              (setf (dummy-node-source v)
                    (list (dummy-node-source v)))))))
  (iter (for layer in-vector (reverse layers))
        (let* ((nodes (cdr layer))
               (v (car nodes)))
          (dolist (x nodes)
            (when (dummy-node-p x)
              (dolist (e (node- x))
                (let ((y (edge-from e)))
                  (when (dummy-node-p y)
                    (setf (dummy-node-source y)
                          (union (dummy-node-source x)
                                 (dummy-node-source y))))))))
          (setf (cdr layer)
                (cons v
                      (iter (for w in (cdr nodes))
                            (cond
                              ((and (node-graph v)
                                    w
                                    (dummy-node-p v)
                                    (dummy-node-p w)
                                    (null
                                     (cdr
                                      (remove-duplicates
                                       (append (node- v)
                                               (node+ v)
                                               (node- w)
                                               (node+ w))
                                       :key #'edge-flip-type)))
                                    (and
                                     (equal (dummy-node-source v)
                                            (dummy-node-source w))
                                     (null (cdr (dummy-node-source v)))))
                               (dolist (e (node- w))
                                 (unless (find-edge (edge-from e) v)
                                   (add-edge (edge-from e) v)))
                               (dolist (e (node+ w))
                                 (unless (find-edge v (edge-to e))
                                   (add-edge v (edge-to e))))
                               (delete-node w))
                              (t
                               (collect w)
                               (setf v w)))))))))

(defun collapse-dummy-nodes/aggressively (layers)
  (iter (for layer in-vector layers)
        (let* ((nodes (cdr layer))
               (v (car nodes)))
          (setf (cdr layer)
                (cons v
                      (iter (for w in (cdr nodes))
                            (cond
                              ((and (node-graph v)
                                    w
                                    (dummy-node-p v)
                                    (dummy-node-p w)
                                    (null
                                     (cdr
                                      (remove-duplicates
                                       (append (node- v)
                                               (node+ v)
                                               (node- w)
                                               (node+ w))
                                       :key #'edge-flip-type))))
                               (dolist (e (node- w))
                                 (unless (find-edge (edge-from e) v)
                                   (add-edge (edge-from e) v)))
                               (dolist (e (node+ w))
                                 (unless (find-edge v (edge-to e))
                                   (add-edge v (edge-to e))))
                               (delete-node w))
                              (t
                               (collect w)
                               (setf v w)))))))))

(defun compute-x-coordinates (g layers)
  (declare (ignore g))
  (let ((max-width
         (iter (for layer in-vector layers)
               (let ((nodes (cdr layer))
                     (x 0))
                 (dolist (v nodes)
                   (setf (node-x v) x)
                   (incf x (1+ (node-width v))))
                 (maximizing x)))))
    (iter (for layer in-vector layers)
          (let* ((nodes (cdr layer))
                 (width (node-x (car (last nodes))))
                 (d (/ (- max-width width) 2)))
            (dolist (node nodes)
              (incf (node-x node) d))))))

(defun foo (layers)
  ;; Sweep forward:
  (iter (for i from 1 to (1- (length layers)))
        (foo-step/new (elt layers i) #'from-nodes))
  ;; And back:
  (iter (for i from (- (length layers) 2) downto 0)
        (foo-step/new (elt layers i) #'to-nodes))
  (let ((min-x
         (iter (for layer in-vector layers)
               (minimizing
                (iter (for node in (cdr layer))
                      (minimizing
                       (- (node-x node)
                          (truncate (node-width node) 2))))))))
    (iter (for layer in-vector layers)
          (iter (for node in (cdr layer))
                (decf (node-x node)
                      min-x))))
  ;; re-sort the layer lists, now that node order has been established:
  (iter (for layer in-vector layers)
        (setf (cdr layer) (sort (copy-list (cdr layer)) #'< :key #'node-x))))

(defparameter *layer-height* 75)
(defparameter *x/y-factor* 1/2)

(defun foo-step/old (layer direction)
  (declare (ignore direction))          ;?!
  (dolist (v (cdr layer))
    (when-let ((reference-nodes (append (from-nodes v) (to-nodes v))))
      (setf (node-x v)
            (- (round (/ (iter (for w in reference-nodes)
                               (sum (+ (node-x w)
                                       (/ (node-width w) 2))))
                         (length reference-nodes)))
               (node-width v)))))
  (let ((extra (* *x/y-factor* *layer-height*)))
    (iter (for (v w) on (sort (copy-list (cdr layer)) #'< :key #'node-x))
          (let ((newval (+ (node-x v)
                           (if (dummy-node-p v)
                               (node-width v)
                               (+ 5 (max (node-width v) extra))))))
            (when (and w (>= newval (node-x w)))
              (setf (node-x w) newval))))))

(defun foo-step/new (layer direction)
  (declare (ignore direction))          ;?!
  (dolist (v (cdr layer))
    (when-let ((reference-nodes #+nil (append (from-nodes v) (to-nodes v))
                                (funcall direction v)))
      (setf (node-x v)
            (- (round (/ (iter (for w in reference-nodes)
                               (sum (+ (node-x w)
                                       (/ (node-width w) 2))))
                         (length reference-nodes)))
               (round (node-width v) 2)))))
  (let ((extra 0 #+nil (* *x/y-factor* *layer-height*)))
    (iter (for (v w) on (sort (copy-list (cdr layer)) #'< :key #'node-x))
          (let ((newval (+ (node-x v)
                           (if (dummy-node-p v)
                               (node-width v)
                               (+ 5 (max (node-width v) extra))))))
            (when (and w (>= newval (node-x w)))
              (setf (node-x w) newval))))))

;;;;
;;;; Drawing
;;;;

(defmethod x ((v node-with-position))
  (round (node-x v)))

(defmethod y ((v sugiyama-node))
  (+ 50 (* *layer-height* (node-layer v))))

(defun font ()
  (#_new QFont "Arial" 11))

(defun outgoing-edge-x (edge)
  (let* ((v (edge-from edge))
         (all-edges (sort (copy-list (node+ v))
                          #'<
                          :key (alexandria:compose
                                #'node-x
                                #'edge-to)))
         (i (position edge all-edges))
         (n (length all-edges)))
    (+ (x v) (truncate (* (1+ i) (node-width v)) (1+ n)))))

(defun outgoing-edge-y (edge)
  (let ((v (edge-from edge)))
    (+ (y v)
       (if (dummy-node-p v)
           0
           (+ (node-height v) #+nil 5)))))

(defun incoming-edge-y (edge)
  (let ((v (edge-to edge)))
    (- (y v)
       (if (dummy-node-p v) 0 5))))

(defun incoming-edge-x (edge)
  (let* ((v (edge-to edge))
         (all-edges (sort (copy-list (node- v))
                          #'<
                          :key (alexandria:compose
                                #'node-x
                                #'edge-from)))
         (i (position edge all-edges))
         (n (length all-edges)))
    (+ (x v) (truncate (* (1+ i) (node-width v)) (1+ n)))))

(defclass clickable-qgraphicspathitem ()
    ((data :initarg :data
           :initform nil
           :accessor item-data)
     (press-callback :initarg :press-callback
                     :initform nil
                     :accessor item-callback))
  (:metaclass qt-class)
  (:qt-superclass "QGraphicsPathItem")
  (:override ("itemChange" item-change/3)
             ("mousePressEvent" mouse-press-event/2)))

(defmethod initialize-instance :after
    ((instance clickable-qgraphicspathitem) &key path pen brush)
  (new instance path)
  (#_setPen instance pen)
  (#_setBrush instance brush))

(defmethod item-change/3 ((item clickable-qgraphicspathitem) change value)
  (when (enum= (#_QGraphicsItem::ItemSceneHasChanged) change)
    (if (typep (#_scene item) 'qt::null-qobject)
        (qt::note-child-removed item)
        (qt::note-child-added item)))
  (call-next-qmethod))

(defmethod mouse-press-event/2 ((item clickable-qgraphicspathitem) event)
  #+nil (#_removeItem (#_scene item) item)
  (with-slots (data press-callback) item
    (when press-callback
      (funcall press-callback data item))))

(defun make-clickable-qgraphicspathitem (path pen brush &rest keys)
  (apply #'make-instance
         'clickable-qgraphicspathitem
         :path path
         :pen pen
         :brush brush
         keys))

(defun add-node-background-path (scene rectf color &rest keys)
  (let* ((path (#_new QPainterPath))
         (gradient (#_new QLinearGradient
                          (#_topLeft rectf)
                          (#_bottomLeft rectf)))
         (pen (#_new QPen (#_new QColor 150 150 150))))
    (#_translate rectf 0 -0.5)          ;avoid anti-aliasing blur
    (#_addRoundedRect path rectf 10 10)
    (#_setColorAt gradient 0 (#_new QColor 255 255 255))
    (#_setColorAt gradient 1 (multiple-value-bind (r g b)
                                 (if color
                                     (values (ldb (byte 8 16) color)
                                             (ldb (byte 8 8) color)
                                             (ldb (byte 8 0) color))
                                     (values 200 255 200))
                               (#_new QColor r g b)))
    (#_setWidthF pen 1)
    (let ((item (apply #'make-clickable-qgraphicspathitem
                       path
                       pen
                       (#_new QBrush gradient)
                       keys)))
      (#_addItem scene item)
      (#_setZValue item 0)
      item)))

(defun add-node-text-item (scene v str font)
  (let ((text-item (#_addSimpleText scene str font)))
    (#_setPos text-item (+ 10 (x v)) (+ (y v)))
    (#_setZValue text-item 1)
    text-item))

(defun avg (a b &optional (f 1/2))
  (+ a (* f (- b a))))

(defun add-arrow-path (scene &key x1 y1 x2 y2 zz1 zz2 from-arrow to-arrow)
  (let ((path (#_new QPainterPath))
        (pen (#_new QPen (#_new QColor 0 0 0))))
    (#_moveTo path x1 y1)
    (#_cubicTo path
               (#_new QPointF x1 (+ y1 (* 2 zz1)))
               (#_new QPointF x2 (+ y2 (* -2 zz2)))
               (#_new QPointF x2 y2))
    (let* ( ;; fixme: need the slope of the spline, not the stract line
           (arrow-line-length 10)
           (magic-number (* 4 zz1))
           (alpha (atan (+ (- y2 y1) magic-number) (- x2 x1 )))
           (beta alpha)
           (delta 0.4))
      (when from-arrow
        (#_moveTo path x1 y1)
        (#_lineTo path
                  (+ x1 (* arrow-line-length (cos (+ beta delta))))
                  (+ y1 (* arrow-line-length (sin (+ beta delta)))))
        (#_moveTo path x1 y1)
        (#_lineTo path
                  (+ x1 (* arrow-line-length (cos (- beta delta))))
                  (+ y1 (* arrow-line-length (sin (- beta delta))))))
      (when to-arrow
        (let ((beta (+ beta pi)))
          (#_moveTo path x2 y2)
          (#_lineTo path
                    (+ x2 (* arrow-line-length (cos (+ beta delta))))
                    (+ y2 (* arrow-line-length (sin (+ beta delta)))))
          (#_moveTo path x2 y2)
          (#_lineTo path
                    (+ x2 (* arrow-line-length (cos (- beta delta))))
                    (+ y2 (* arrow-line-length (sin (- beta delta))))))))
    (let ((item (#_addPath scene path pen)))
      (#_setZValue item 2)
      item)))

(defun node-pressed (node item)
  (cond
    ((eq (node-key node) :root)
     (qt-hemlock::add-project-to-graph-command nil)
     (clear-echo-area)
     (;; fixme: eats away the RET, is there a better way?
      hemlock::trash-character)
     (qt-hemlock::redraw-needed))
    (t
     (let ((graph (node-graph node))
           (scene (#_scene item)))
       (update-graph-and-scene (lambda ()
                                 (delete-node node)
                                 (prune (find-node :root graph)))
                               graph
                               scene)))))

(defun update-graph-and-scene (fn graph scene)
  (let ((prev (make-static-graph graph)))
    (unsugiyama graph)
    (funcall fn)
    (sugiyama-for-scene graph scene)
    (let ((new (make-static-graph graph))
          (n #+nil 4 20))
      (multiple-value-bind (tmp node-specs)
          (make-interpolation-graph prev new)
        (static-graph-to-scene tmp scene)
        (iter (for i from 1 to n)
              (update-interpolation-graph node-specs (/ i n))
              (qt-hemlock::dispatch-events-no-hang)))
      (static-graph-to-scene new scene))))

(defun make-interpolation-graph (a b)
  (let ((c (make-static-graph a)))
    (values
     c
     (iter (for v in-graph c)
           (let* ((key (if (dummy-node-p v)
                           (destructuring-bind (p q i) (cdr (node-key v))
                             (list :dummy p q i))
                           (node-key v)))
                  (old (find-node key a))
                  (new (find-node key b nil)))
             (cond
               (new
                (collect (list old v new)))
               (t
                (delete-node v))))))))

(defun update-interpolation-graph (node-list f)
  (iter (for (old v new) in node-list)
        (let ((new-x (avg (node-x old) (node-x new) f))
              (new-y (round (avg (y old) (y new) f))))
          (let ((dx (round (- new-x (node-x v))))
                (dy (round (- new-y (y v)))))
            (dolist (item (node-graphics-items v))
              (#_translate item dx dy))
            (dolist (e (node+ v))
              (#_translate (edge-graphics-item e) dx dy)))
          (setf (node-x v) new-x)
          (setf (y v) new-y))))

(defun graph-to-scene (graph scene)
  (sugiyama-for-scene graph scene)
  (static-graph-to-scene (make-static-graph graph) scene))

(defun sugiyama-for-scene (graph scene)
  (declare (ignore scene))
  (let* ((font (font))
         (metrics (#_new QFontMetrics font)))
    (sugiyama graph
              (lambda (str)
                (values
                 (+ 20 (#_width (#_boundingRect metrics str)))
                 (+ (#_height (#_boundingRect metrics str))
                    (#_descent metrics))))
              :collapse nil)))

(defun static-graph-to-scene (static-graph scene)
  (#_clear scene)
  (let* ((font (font))
         (metrics (#_new QFontMetrics font)))
    (map-nodes
     (lambda (v)
       (unless (dummy-node-p v)
         (let ((label (or (node-label v)
                          (node-key v))))
           (let ((rect (#_boundingRect metrics label)))
             (#_translate rect
                          (x v)
                          (+ (y v)
                             (truncate (#_height rect) 2)
                             (#_descent metrics)))
             (#_setWidth rect (+ (#_width rect) 20))
             (setf (node-graphics-items v)
                   (list
                    (add-node-background-path scene
                                              (#_new QRectF rect)
                                              (node-color v)
                                              :data (node-original-node v)
                                              :press-callback #'node-pressed)
                    (add-node-text-item scene v label font))))))
       (add-node-edge-items scene v))
     static-graph)))

(defun add-node-edge-items (scene v)
  (iter (for e in (node+ v))
        (for i from 0)
        (let ((w (edge-to e)))
          (setf (edge-graphics-item e)
                (add-arrow-path scene
                                :x1 (outgoing-edge-x e)
                                :y1 (outgoing-edge-y e)
                                :x2 (incoming-edge-x e)
                                :y2 (incoming-edge-y e)
                                :zz1 (if (dummy-node-p v) 14 5)
                                :zz2 (if (dummy-node-p w) 14 5)
                                :from-arrow (and (edge-flip-type e)
                                                 (null (dummy-node-p v)))
                                :to-arrow (and (not (eq (edge-flip-type e)
                                                        :extra))
                                               (null (dummy-node-p w))))))))


;;;;
;;;; Hemlock integration
;;;;

#|
(defmode "Graph" :major-p t)

(defun graph-scroll-page (f area)
  (let ((scroller (#_verticalScrollBar area)))
    (#_setValue scroller (+ (#_value scroller)
                            (* f (#_height area))))))

(defcommand "Graph Page Down" (p)
  "" ""
  (declare (ignore p))
  (graph-scroll-page 1 (hi::buffer-widget (current-buffer))))

(defcommand "Graph Page Up" (p)
  "" ""
  (declare (ignore p))
  (graph-scroll-page -1 (hi::buffer-widget (current-buffer))))

(defun graph-scroll-fixed (c area)
  (let ((scroller (#_verticalScrollBar area)))
    (#_setValue scroller (+ c (#_value scroller)))))

(defcommand "Graph Scroll Down" (p)
  "" ""
  (declare (ignore p))
  (graph-scroll-fixed 20 (hi::buffer-widget (current-buffer))))

(defcommand "Graph Scroll Up" (p)
  "" ""
  (declare (ignore p))
  (graph-scroll-fixed -20 (hi::buffer-widget (current-buffer))))

(defun graph-scroll-to (f area)
  (let ((scroller (#_verticalScrollBar area)))
    (#_setValue scroller
                (* f
                   (max 0
                        (+ (#_maximum scroller)
                           (#_height area)))))))

(defcommand "Graph Scroll Top" (p)
  "" ""
  (declare (ignore p))
  (graph-scroll-to 0 (hi::buffer-widget (current-buffer))))

(defcommand "Graph Scroll Bottom" (p)
  "" ""
  (declare (ignore p))
  (graph-scroll-to 1 (hi::buffer-widget (current-buffer))))

(bind-key "Graph Page Down" #k"space" :mode "Graph")
(bind-key "Graph Page Down" #k"control-v" :mode "Graph")
(bind-key "Graph Page Up" #k"meta-v" :mode "Graph")
(bind-key "Graph Page Up" #k"backspace" :mode "Graph")

(bind-key "Graph Scroll Down" #k"control-n" :mode "Graph")
(bind-key "Graph Scroll Up" #k"control-p" :mode "Graph")

(bind-key "Graph Scroll Top" #k"meta-\<" :mode "Graph")
(bind-key "Graph Scroll Bottom" #k"meta-\>" :mode "Graph")

(bind-key "Enter Foreign Widget" #k"enter" :mode "Graph")

|#

(defclass asdf-info ()
  ((name :initarg :name
         :accessor asdf-name)
   (deps :initarg :deps
         :accessor asdf-deps)))

(defun asdf-info ()
  (let ((g (make-instance 'graph
                          :test 'equal
                          :node-class 'sugiyama-node
                          :edge-class 'sugiyama-edge))
        (table (make-hash-table :test 'equal)))
    (with-open-file (s p)
      (iter (for line = (read-line s nil))
            (while line)
            (destructuring-bind (project &rest deps)
                (cl-ppcre:split " " line)
              (unless (equal project "#")
                (setf (gethash project table)
                      (make-instance 'asdf-info
                                     :name project
                                     :deps deps))
                (mapcar (lambda (name)
                          )
                        (asdf-deps info))))))
    (iter (for (nil info) in-hashtable table)
          (setf (asdf-deps info)
                (mapcar (lambda (name)
                          )
                        (asdf-deps info)))
          (let ((v (intern-node project g)))
            (dolist (dep deps)
              (unless (or (find-edge v (intern-node dep g))
                          (some (lambda (other)
                                  (find dep (gethash other table)
                                        :test #'equal))
                                deps))
                (add-edge v (intern-node dep g))))))
    g))

(defun load-dependencies (p)
  (let ((g (make-instance 'graph
                          :test 'equal
                          :node-class 'sugiyama-node
                          :edge-class 'sugiyama-edge))
        (table (make-hash-table :test 'equal)))
    (with-open-file (s p)
      (iter (for line = (read-line s nil))
            (while line)
            (destructuring-bind (project &rest deps)
                (cl-ppcre:split " " line)
              (unless (equal project "#")
                (setf (gethash project table) deps)))))
    (iter (for (project deps) in-hashtable table)
          (let ((v (intern-node project g)))
            (dolist (dep deps)
              (unless (or (find-edge v (intern-node dep g))
                          (some (lambda (other)
                                  (find dep (gethash other table)
                                        :test #'equal))
                                deps))
                (add-edge v (intern-node dep g))))))
    g))

(defun merge-node-into (node into-graph)
  (intern-node (node-key node)
               into-graph
               :label (node-label node)
               :color (node-color node)))

(defun merge-graph-into (graph into-graph)
  (iter (for v in-graph graph)
        (merge-node-into v into-graph)
        (dolist (e (node+ v))
          (let ((from (merge-node-into (edge-from e) into-graph))
                (to (merge-node-into (edge-to e) into-graph)))
            (unless (find-edge from to)
              (add-edge from to))))))

(defun clbuild-dependency-graph
    (&optional projects (p "/home/david/clbuild-tmp/dependencies"))
  (let* ((g (load-dependencies p))
         (main
          (intern-node :root
                       g
                       :label "project dependencies (click to add)")))
    (dolist (project projects)
      (add-edge main (find-node project g)))
    (prune main)
    g))
