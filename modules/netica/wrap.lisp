;;; Lisp wrappers for the Netica API
;;; <http://norsys.com/netica_c_api.htm>

(require "netica")

(defpackage "NETICA"
  (:export "null-pointer" "*verbose*" "*env*" "error-category" "error-message"
           "check-errors" "start-netica" "close-netica" "save-net"
           "make-net" "make-node" "get-beliefs" "enter-finding"))

(in-package "SYS")
(eval-when (compile load eval)
  (setf (package-lock "SYS") nil))

(let ((null nil))
(defun netica:null-pointer ()
  "return a null foreign pointer "
  (unless (and null (ffi:validp null))
    (setq null (ffi:unsigned-foreign-address 0)))
  null))

(defvar netica:*verbose* *standard-output* "the netica log stream")
(defvar netica:*env* nil "the current netica environment")

(defun netica:error-category (err)
  "return the list of categories where the error belongs"
  (mapcan (lambda (c)
            (unless (zerop (netica::ErrorCategory_ns (symbol-value c) err))
              (list c)))
          '(netica::OUT_OF_MEMORY_CND
            netica::USER_ABORTED_CND
            netica::FROM_WRAPPER_CND
            netica::FROM_DEVELOPER_CND
            netica::INCONS_FINDING_CND)))

(defun netica:error-message (err)
  "Convert netica error to a string"
  (format nil "~s/~s ~s: ~s~%"
          (netica::ErrorSeverity_ns err)
          (netica::ErrorNumber_ns err)
          (netica::error-category err)
          (netica::ErrorMessage_ns err)))

(defun netica:check-errors (&key ((:env netica:*env*) netica:*env*) (clear t)
                            (severity netica::NOTHING_ERR))
  "Check all errors of the given severity and optionally clear them."
  (let ((err (netica:null-pointer)))
    (loop (setq err (netica::GetError_ns netica:*env* severity err))
      (when (ffi:foreign-address-null err) (return))
      (if (>= (netica::ErrorSeverity_ns err) netica::ERROR_ERR)
          (error (netica:error-message err))
          (warn (netica:error-message err)))
      (when clear (netica::ClearError_ns err)))))

(defun netica:start-netica (license &key
                            ((:verbose netica:*verbose*) netica:*verbose*))
  "Start netica, initialize it, and return the new environment.
Sets netica:*env* to this environment on success."
  (let ((env (netica::NewNeticaEnviron_ns license (netica:null-pointer) nil))
        status message)
    (when netica:*verbose*
      (format netica:*verbose* "~&;; new environment: ~s~%" env))
    (multiple-value-setq (status env message) (netica::InitNetica_bn env))
    (when netica:*verbose*
      (format netica:*verbose* ";; init status=~s~%~a~%" status message))
    (multiple-value-setq (status message) (netica::GetNeticaVersion_bn env))
    (when netica:*verbose*
      (format netica:*verbose* ";; version=~s (~s)~%" status message))
    (setq status (netica::ArgumentChecking_ns netica::REGULAR_CHECK env))
    (when netica:*verbose*
      (format netica:*verbose* ";; checking level: ~s --> ~s~%"
              status netica::REGULAR_CHECK))
    (setq status (netica::MaxMemoryUsage_ns
                  (float netica::QUERY_CHECK 1d0) env))
    (when netica:*verbose*
      (format netica:*verbose* ";; memory usage: ~s bytes~%" status))
    (netica:check-errors :env env)
    (setq netica:*env* env)))

(defun netica:close-netica (&key (env netica:*env*)
                            ((:verbose netica:*verbose*) netica:*verbose*))
  "Terminate the netica session.
Sets netica:*env* to NIL when it was closed."
  (netica:check-errors)
  (multiple-value-bind (status message) (netica::CloseNetica_bn netica:*env*)
    (when netica:*verbose*
      (format netica:*verbose* "~&;; close status=~s~%~a~%" status message)))
  (when (eq env netica:*env*)
    (setq netica:*env* nil)))

(defun required-argument (f a) (error "~s: missing ~s argument" f a))

(defun netica:make-net (&key (name (symbol-name (gensym)))
                        ((:env netica:*env*) netica:*env*)
                        ((:verbose netica:*verbose*) netica:*verbose*))
  "Make a network with a given name and return it."
  (let ((net (netica::NewNet_bn name netica:*env*)))
    (when netica:*verbose*
      (format netica:*verbose* "~&;; new net ~s: ~s~%" name net))
    (netica:check-errors)
    net))

(defun netica:make-node (&key (name (symbol-name (gensym)))
                         (net (required-argument 'netica:make-node :net))
                         (kind netica::NATURE_NODE)
                         (states nil) (num-states (length states))
                         (title nil) (comment nil)
                         (parents nil) (cpt nil)
                         ((:env netica:*env*) netica:*env*)
                         ((:verbose netica:*verbose*) netica:*verbose*))
  "Make a network node with the given parameters and return it.
The parameters are: name, net, kind, states (state name list),
number of states, parents list, cpt.\
CMP (conditional probability table) is a list of conses:
 ((parent-state-vector . node-state-probability-vector) ...)
one cons for each combination of possible parent states,
where parent-state-vector is a vector of parent states,
 its length being (length parents);
and node-state-probability-vector is a vector of corresponding node state
 probabilities, its length being (length states)."
  (let ((node (netica::NewNode_bn name num-states net)))
    (when netica:*verbose*
      (format netica:*verbose* "~&;; new node ~s: ~s~%" name node))
    (netica:check-errors)
    (when (/= kind netica::NATURE_NODE)
      (netica::SetNodeKind_bn node kind)
      (netica:check-errors))
    (loop :for state :in states :and idx :upfrom 0
      :do (if (consp state)
              (progn
                (netica::SetNodeStateName_bn node idx (car state))
                (netica::SetNodeStateTitle_bn node idx (cdr state)))
              (netica::SetNodeStateName_bn node idx state))
      (netica:check-errors))
    (when title
      (netica::SetNodeTitle_bn node title)
      (netica:check-errors))
    (when comment
      (netica::SetNodeComment_bn node comment)
      (netica:check-errors))
    (dolist (parent parents)
      (netica::AddLink_bn parent node)
      (netica:check-errors))
    (dolist (probs cpt)
      (netica::SetNodeProbs_bn node (car probs) (cdr probs))
      (netica:check-errors))
    (netica:check-errors)
    node))

(defun netica:get-beliefs (node &key
                           ((:env netica:*env*) netica:*env*)
                           ((:verbose netica:*verbose*) netica:*verbose*))
  "Get the belief vector for the node."
  (let ((beliefs (netica::GetNodeBeliefs_bn node))
        (name (netica::GetNodeName_bn node)))
    (netica:check-errors)
    (when netica:*verbose*
      (loop :for belief :across beliefs :and index :upfrom 0 :do
        (format netica:*verbose* "~&;; ~a: P(~s)=~f~%" name
                (netica::GetNodeStateName_bn node index) belief))
      (netica:check-errors))
    beliefs))

(defun netica:enter-finding (net node state &key
                             ((:env netica:*env*) netica:*env*)
                             ((:verbose netica:*verbose*) netica:*verbose*))
  "Enter a finding by node and state names"
  (let* ((nd (netica::NodeNamed_bn node net))
         (st (netica::StateNamed_bn state nd)))
    (netica::EnterFinding_bn nd st)
    (netica:check-errors)
    (when netica:*verbose*
      (format netica:*verbose* "~&;; ~s: set to ~s~%" node state))))

(defun netica:save-net (file net &key
                        ((:env netica:*env*) netica:*env*)
                        ((:verbose netica:*verbose*) netica:*verbose*))
  "Save the network to the file."
  (let ((out (netica::NewStreamFile_ns
              (namestring (merge-pathnames file ".dne"))
              netica:*env* (netica:null-pointer))))
    (when netica:*verbose*
      (format netica:*verbose* "~&;; new stream: ~s~&" out))
    (netica:check-errors)
    (netica::WriteNet_bn net out)
    (netica:check-errors)
    (when netica:*verbose*
      (format netica:*verbose* ";; saved ~s to ~s (~s)~%" net
              (netica::GetNetFileName_bn net) out))))

(push "NETICA" *system-package-list*)
(eval-when (compile load eval)
  (setf (package-lock *system-package-list*) t))
