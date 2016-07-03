(defmop I-m-Al (m-actor) instance)
(defmop I-m-Chuck (m-actor) instance)
(defmop I-m-David (m-actor) instance)
(defmop I-m-Randy (m-actor) instance)
(defmop I-m-Ted (m-actor) instance)
(defmop I-m-Tim (m-actor) instance)

(defmop m-frequency (m-root) (severity nil))
(defmop I-m-once (m-frequency) (severity 0))
(defmop I-m-several-times (m-frequency) (severity 1))
(defmop I-m-repeatedly (m-frequency) (severity 2))

(defmop m-motive (m-root))
(defmop m-justified (m-motive))
(defmop m-unjustified (m-motive))
(defmop I-m-self-defence (m-justified) instance)
(defmop I-m-retaliation (m-unjustified) instance)
(defmop I-m-unprovoked (m-unjustified) instance)

(defmop m-crime-type (m-root))
(defmop I-m-homicide (m-crime-type) instance)

(defmop range-constraint (constraint-fn))
(defmop m-range (m-pattern)
  (abst-fn range-constraint))
(defun range-constraint (constraint filler slots)
  (declare (ignore slots))
  (and (numberp filler)
       (let ((below (role-filler 'below constraint))
             (above (role-filler 'above constraint)))
         (and (or (null below) (< filler below))
              (or (null above) (< above filler))))))

(defmop m-fight-act (m-act) (severity nil))
(defmop m-hurt-act (m-fight-act)
  (severity m-range (below 5)))
(defmop I-m-slap (m-hurt-act) (severity 1))
(defmop I-m-hit (m-hurt-act) (severity 1))
(defmop I-m-strike (m-hurt-act) (severity 2))
(defmop I-m-knock-down (m-hurt-act) (severity 3))
(defmop I-m-slash (m-hurt-act) (severity 4))

(defmop m-wound-act (m-fight-act)
  (severity m-range (above 4)))
(defmop I-m-stab (m-wound-act) (severity 5))
(defmop I-m-shoot (m-wound-act) (severity 5))
(defmop I-m-break-skull (m-wound-act) (severity 5))

(defmop m-state (m-root))
(defmop m-phys-state (m-state) (severity nil))
(defmop I-m-bruised (m-phys-state) (severity 1))
(defmop I-m-knocked-down (m-phys-state) (severity 2))
(defmop I-m-cut (m-phys-state) (severity 3))
(defmop I-m-dead (m-phys-state) (severity 5))

(defmop m-outcome (m-root))
(defmop m-fight-outcome (m-outcome)
  (state m-phys-state) (actor m-actor))

(defmop m-fight-event (m-event)
  (action m-fight-act))

(defmop m-event-group (m-group) (1 m-event))
(defmop m-outcome-group (m-group) (1 m-outcome))
(defmop m-escalation-group (m-group) (1 m-range))
(defmop m-motive-group (m-group) (1 m-motive))

(defmop calc-escalations (m-function))
(defmop calc-motives (m-function))
(defmop adapt-sentence (m-function))
(defmop calc-sentence (m-function))

(defmop m-crime (m-case)
  (crime-type m-crime-type)
  (defendant m-actor)
  (victim m-actor)
  (events m-event-group)
  (outcomes m-outcome-group)
  (escalations m-pattern (calc-fn calc-escalations))
  (motives m-pattern (calc-fn calc-motives))
  (sentence m-pattern (calc-fn adapt-sentence)))

(defun calc-escalations (pattern mop)
  (declare (ignore pattern))
  (format t "~&----------------")
  (format t "~&Calculating escalations in ~S" mop)
  (list->group
   (let ((prev-severity 0))
     (for (event :in (group->list
                      (role-filler 'events mop)))
          :save (let ((this-severity
                       (path-filler '(action severity)
                                    event)))
                  (let ((result
                         (- this-severity prev-severity)))
                    (setf prev-severity this-severity)
                    result))))))

(defun calc-motives (pattern mop)
  (declare (ignore pattern))
  (format t "~&----------------")
  (format t "~&Calculating motives in ~S" mop)
  (list->group
   (let ((prev-motive 0))
     (for (escalation :in (group->list
                           (get-filler 'escalations mop)))
          :save (setf prev-motive
                  (mop-calc
                   `((role motive)
                     (escalation ,escalation)
                     (prev-motive ,prev-motive)
                     )))))))

(defun mop-calc (slots)
  (let ((instance (slots->mop slots '(m-calc) nil)))
    (and instance
         (get-filler 'value instance))))

(defmop motive (m-role) instance)

(defmop m-calc (m-root))

(defmop m-calc-motive (m-calc)
  (role motive) (value nil))

(defmop m-calc-escalation-motive (m-calc-motive)
  (escalation m-range (above 0))
  (value I-m-retaliation))

(defmop m-calc-self-defenece-motive (m-calc-motive)
  (escalation m-range (below 1))
  (prev-motive m-unjustified)
  (value I-m-self-defence))

(defmop m-calc-retaliation-motive (m-calc-motive)
  (escalation m-range (below 1))
  (prev-motive m-justified)
  (value I-m-retaliation))

(defun adjust-sentence (pattern mop)
  (declare (ignore pattern))
  (format t "~&----------------")
  (format t "~&~S applied, ~S events from the end"
    mop (get-filler 'index mop))
  (adjust-fn
   (get-filler 'old-sentence mop)
   (get-filler 'weight mop)
   (get-filler 'index mop)
   (get-filler 'direction mop)))

(defun adjust-fn (x y index direction)
  (+ x (* x 
          (+ y (cond ((< index 2) 0.25) (t 0.0)))
          direction)))

(defun adapt-sentence (pattern mop)
  (declare (ignore pattern))
  (let ((old-mop (get-filler 'old mop)))
    (let ((old-size (group-size
                     (get-filler 'events old-mop)))
          (size (group-size (get-filler 'events mop)))
          (old-sentence (get-filler 'sentence old-mop)))
      (format t "~&----------------")
      (format t "~&Adapting the sentence in ~S" old-mop)
      (or (for (old-pos :in (make-m-n old-size 1))
               (pos :in (make-m-n size 1))
               :first
               (mop-calc
                `((role sentence)
                  (index ,(- size pos))
                  (old-sentence ,old-sentence)
                  ,@(crime-compare-slots
                     old-mop old-pos
                     '(old-action old-motive old-severity))
                  ,@(crime-compare-slots
                     mop pos
                     '(this-action this-motive this-severity)))))
          (progn (format t "~&----------------")
            (format t "~&No major difference found")
            (format t "~&Using old sentence")
            old-sentence)))))

(defun crime-compare-slots (mop pos roles)
  (let ((paths `((events ,pos action)
                 (motives ,pos)
                 (outcomes ,pos state severity))))
    (insist crime-compare-slots
            (eql (length roles) (length paths)))
    (for (role :in roles) (path :in paths)
         :save (make-slot role (path-filler path mop)))))

(defmop compare-constraint (constraint-fn))

(defmop m-compare (m-pattern)
  (abst-fn compare-constraint) (to m-role)
  (compare-fn m-function))

(defmop eql (m-function))
(defmop < (m-function))

(defmop m-equal (m-compare) (compare-fn eql))
(defmop m-less-than (m-compare) (compare-fn <))

(defun compare-constraint (constraint filler slots)
  (funcall (get-filler 'compare-fn constraint)
           filler
           (indirect-filler 'to constraint slots)))

(defun indirect-filler (role mop slots)
  (get-filler (get-filler role mop) slots))

(defmop sentence (m-role) instance)
(defmop old-severity (m-role) instance)
(defmop this-severity (m-role) instance)
(defmop adjust-sentence (m-function))

(defmop m-adapt-sentence (m-calc)
  (role sentence)
  (value m-pattern (calc-fn adjust-sentence)))

(defmop m-adapt-extreme-force-old (m-adapt-sentence)
  (old-action m-wound-act)
  (this-action m-not (object m-wound-act))
  (old-motive m-unjustified)
  (this-motive m-unjustified)
  (weight 0.50) (direction -1))

(defmop m-adapt-extreme-force-new (m-adapt-sentence)
  (old-action m-not (object m-wound-act))
  (this-action m-wound-act)
  (old-motive m-unjustified)
  (this-motive m-unjustified)
  (weight 0.50) (direction 1))

(defmop m-adapt-worse-motive-old (m-adapt-sentence)
  (old-severity nil)
  (this-severity m-equal (to old-severity))
  (old-motive m-unjustified)
  (this-motive m-justified)
  (weight 0.25) (direction -1))

(defmop m-adapt-worse-motive-new (m-adapt-sentence)
  (old-severity nil)
  (this-severity m-equal (to old-severity))
  (old-motive m-justified)
  (this-motive m-unjustified)
  (weight 0.25) (direction 1))

(defmop m-adapt-mixed-old (m-adapt-sentence)
  (old-severity nil)
  (this-severity m-less-than (to old-severity))
  (old-motive m-justified)
  (this-motive m-unjustified)
  (weight 0.00) (direction -1))

(defmop m-adapt-mixed-new (m-adapt-sentence)
  (this-severity nil)
  (old-severity m-less-than (to this-severity))
  (old-motive m-unjustified)
  (this-motive m-justified)
  (weight 0.00) (direction 1))

(defparameter *case1*
  '((crime-type I-m-homicide)
    (defendant I-m-Ted) (victim I-m-Al)
    (events m-group
            (1 m-fight-event
             (action I-m-slash)
             (actor I-m-Ted) (object I-m-Al)
             (freq I-m-once))
            (2 m-fight-event
             (action I-m-slash)
             (actor I-m-Al) (object I-m-Ted)
             (freq I-m-once))
            (3 m-fight-event
             (action I-m-stab)
             (actor I-m-Ted) (object I-m-Al)
             (freq I-m-repeatedly)))
    (outcomes m-group
              (1 m-fight-outcome
               (state I-m-cut) (actor I-m-Al))
              (2 m-fight-outcome
               (state I-m-cut) (actor I-m-Ted))
              (3 m-fight-outcome
               (state I-m-dead) (actor I-m-Al)))
    (sentence 40)))

(defparameter *case2*
  '((crime-type I-m-homicide)
    (defendant I-m-Randy) (victim I-m-Chuck)
    (events m-group
            (1 m-fight-event
             (action I-m-strike)
             (actor I-m-Randy) (object I-m-Chuck)
             (freq I-m-repeatedly))
            (2 m-fight-event
             (action I-m-strike)
             (actor I-m-Chuck) (object I-m-Randy)
             (freq I-m-repeatedly))
            (3 m-fight-event
             (action I-m-slash)
             (actor I-m-Randy) (object I-m-Chuck)
             (freq I-m-once))
            (4 m-fight-event
             (action I-m-slash)
             (actor I-m-Chuck) (object I-m-Randy)
             (freq I-m-once))
            (5 m-fight-event
             (action I-m-stab)
             (actor I-m-Randy) (object I-m-Chuck)
             (freq I-m-repeatedly)))
    (outcomes m-group
              (1 m-fight-outcome
               (state I-m-bruised) (actor I-m-Chuck))
              (2 m-fight-outcome
               (state I-m-bruised) (actor I-m-Randy))
              (3 m-fight-outcome
               (state I-m-cut) (actor I-m-Chuck))
              (4 m-fight-outcome
               (state I-m-cut) (actor I-m-Randy))
              (5 m-fight-outcome
               (state I-m-dead) (actor I-m-Chuck)))))

(defparameter *case3*
  '((crime-type I-m-homicide)
    (defendant I-m-Tim) (victim I-m-David)
    (events m-group
            (1 m-fight-event
             (action I-m-slap)
             (actor I-m-David) (object I-m-Tim)
             (freq I-m-several-times))
            (2 m-fight-event
             (action I-m-strike)
             (actor I-m-Tim) (object I-m-David)
             (freq I-m-several-times))
            (3 m-fight-event
             (action I-m-knock-down)
             (actor I-m-David) (object I-m-Tim)
             (freq I-m-once))
            (4 m-fight-event
             (action I-m-stab)
             (actor I-m-Tim) (object I-m-David)
             (freq I-m-several-times)))
    (outcomes m-group
              (1 m-fight-outcome
               (state I-m-bruised) (actor I-m-Tim))
              (2 m-fight-outcome
               (state I-m-bruised) (actor I-m-David))
              (3 m-fight-outcome
               (state I-m-knocked-down) (actor I-m-Tim))
              (4 m-fight-outcome
               (state I-m-dead) (actor I-m-David)))))

(defun judge-demo ()
  (run-judge *case1* '*case1*)
  (run-judge *case2* '*case2*)
  (run-judge *case3* '*case3*))

(defun run-judge (case case-name)
  (format t "~&----------------")
  (format t "~&Sentencing ~S in ~S"
    (role-filler 'defendant case) case-name)
  (let ((instance (judge (forms->slots case))))
    (insist judge-demo (not (null instance)))
    (format t "~&Sentence in ~S is ~S years"
      instance (role-filler 'sentence instance))
    instance))

(defun judge (slots)
  (let ((instance (slots->mop slots '(m-crime) t)))
    (and (get-filler 'sentence instance)
         instance)))