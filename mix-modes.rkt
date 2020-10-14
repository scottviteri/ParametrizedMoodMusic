(require rsound)
(require srfi/1)
(require rsound/piano-tones)


(define root 69)
(define major-scale (circular-list 2 2 1 2 2 2 1))
(define (repeat f n) (apply compose (make-list n f)))
(define (cumsum lst)
  (let ([csum (unfold (lambda (p) (>= (cdr p) (length lst)))
                      car
                      (lambda (p) (let ([lastsum (car p)]
                                   [i (cdr p)])
                               (cons (+ lastsum (list-ref lst i)) (+ i 1))))
                      (cons 0 0))])
    (reverse (cons (+ (last csum) (last lst)) (reverse csum)))))


(define (get-scale-notes mode)
  (let* ([mode-scale ((repeat cdr mode) major-scale)]
         [accum-intervals (drop (cumsum (take mode-scale 12)) 5)])
    (map (curry + root) accum-intervals)))

; mode 0 = major, 1 = dorian, ...
; scale-degree 0 <-- I chord, 1 <-- II chord, ...
(define (get-chord-notes mode scale-degree)
  (let* ([mode-scale ((repeat cdr mode) major-scale)]
         [accum-intervals (drop (cumsum (take mode-scale (+ 8 scale-degree))) scale-degree)]
         [triad-intervals (map (curry list-ref accum-intervals) '(0 2 4 6))])
    (map (curry + root) triad-intervals)))

(define (random-chord-prog)
  (let ([prog-length (+ 3 (random 4))])
    (cons 0 (map (lambda (_) (+ 1 (random 6))) (iota prog-length)))))

(define (get-chord-rsound mode scale-degree)
  (let* ([chord-notes (get-chord-notes mode scale-degree)]
         [chord-times (map (curry * 1000) (iota (length chord-notes)))])
    (assemble (zip (map piano-tone chord-notes) chord-times))))

(define (get-chord-prog-rsound mode chord-prog wait-frames)
  (let* ([chord-rsounds (map (curry get-chord-rsound mode) chord-prog)]
         [chord-times (map (curry * wait-frames) (iota (length chord-prog)))])
    (assemble (zip chord-rsounds chord-times))))


(define (random-from-list lst)
  (list-ref lst (random (length lst))))

(define (get-notes-rsound mode wait-frames)
  (let* ([scale-notes (get-scale-notes mode)]
         [random-notes (list-tabulate 40 (lambda (_) (random-from-list scale-notes)))]
         [random-note-rsounds (map piano-tone random-notes)]
         [note-times (map (curry * wait-frames) (iota (length random-notes)))])
    (assemble (zip random-note-rsounds note-times))))

(define (loop-rsound rs wait-frames)
  (define rs-length (rs-frames rs))
  (set! wait-frames (if (= 0 wait-frames) rs-length wait-frames))
  (define ps (make-pstream))
  (define counter 0)
  (define (loop-rsound-aux)
    (pstream-queue ps rs (* counter wait-frames))
    (set! counter (+ counter 1))
    (pstream-queue-callback ps loop-rsound-aux (- (* counter wait-frames) 100)))
  (loop-rsound-aux))

; ionian, dorian, phrygian, lydian, mixolydian, minor, locrian
(let* ([melody-mode 1]
       [chord-mode 1]
       [notes-wait 11025]
       [chords-wait (* 2 FRAME-RATE)]
       [notes-rsound (get-notes-rsound melody-mode notes-wait)]
       [chord-prog (random-chord-prog)]
       [chord-prog-rsound (get-chord-prog-rsound chord-mode chord-prog chords-wait)])
  (loop-rsound notes-rsound (* notes-wait 40))
  (loop-rsound chord-prog-rsound (* chords-wait (length chord-prog))))

;(stop)
