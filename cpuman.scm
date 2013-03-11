(module cpuman

  (cpus-available cpus-online cpu-online? cpus-offline
   set-cpu-online! set-cpu-offline!
   set-cpus-online! set-cpus-offline!
   cpu-governor set-cpu-governor! set-cpus-governor! valid-governor?
   cpu-frequency set-cpu-frequency! set-cpus-frequency!
   cpu-min-frequency cpu-max-frequency
   parse-set
  )

(import scheme chicken)
(use srfi-1 srfi-13 posix data-structures extras files)
(use matchable)

(define (read-cpu-line path . args)
  (with-input-from-file
      (apply sprintf (make-pathname "/sys/devices/system/cpu" path) args)
    read-line))

(define (write-cpu-line line path . args)
  (with-output-to-file
      (apply sprintf (make-pathname "/sys/devices/system/cpu" path) args)
    (cut print line)))

(define (range? thing)
  (and (substring-index "-" thing) #t))

(define (parse-range range)
  (let* ((range (map string->number (string-split range "-")))
         (from (car range))
         (to (cadr range)))
    (iota (+ 1 (- to from)) from)))

(define (parse-set set)
  (let ((parts (string-split set ",")))
    (let loop ((parts parts))
      (if (null? parts)
          '()
          (let ((part (car parts)))
            (if (range? part)
                (append (parse-range part)
                        (loop (cdr parts)))
                (cons (string->number part)
                      (loop (cdr parts)))))))))

(define (cpus-available)
  (parse-set (read-cpu-line "present")))

(define (cpus-online)
  (parse-set (read-cpu-line "online")))

(define (cpu-online? cpu)
  (and (memq cpu (cpus-online)) #t))

(define (cpus-offline)
  (parse-set (read-cpu-line "offline")))

(define (set-cpu-online/offline! cpu online?)
  (unless (zero? cpu) ;; cpu 0 is always online
    (write-cpu-line (if online? 1 0) "cpu~a/online" cpu)))

(define (set-cpu-online! cpu)
  (set-cpu-online/offline! cpu #t))

(define (set-cpu-offline! cpu)
  (set-cpu-online/offline! cpu #f))

(define (set-cpus-online! set)
  (for-each set-cpu-online! set))

(define (set-cpus-offline! set)
  (for-each set-cpu-offline! set))

(define valid-governors
  '("ondemand" "powersave" "performance" "conservative" "userspace"))

(define (valid-governor? governor)
  (and (member governor valid-governors) #t))

(define (cpu-governor cpu)
  (read-cpu-line "cpu~a/cpufreq/scaling_governor" cpu))

(define (set-cpu-governor! cpu governor)
  (write-cpu-line governor "cpu~a/cpufreq/scaling_governor" cpu))

(define (set-cpus-governor! set governor)
  (let ((online (cpus-online)))
    (for-each (lambda (cpu)
                (when (memq cpu online)
                  (set-cpu-governor! cpu governor)))
              set)))

(define (cpu-frequency cpu)
  (string->number (read-cpu-line "cpu~a/cpufreq/cpuinfo_cur_freq" cpu)))

(define (cpu-max-frequency cpu)
  (string->number (read-cpu-line "cpu~a/cpufreq/cpuinfo_max_freq" cpu)))

(define (cpu-min-frequency cpu)
  (string->number (read-cpu-line "cpu~a/cpufreq/cpuinfo_min_freq" cpu)))

(define (set-cpu-frequency! cpu frequency)
  (write-cpu-line frequency "cpu~a/cpufreq/cpuinfo_cur_freq" cpu))

(define (set-cpus-frequency! set frequency)
  (let ((online (cpus-online)))
    (for-each (lambda (cpu)
                (when (memq cpu online)
                  (set-cpu-frequency! cpu frequency)))
              set)))

) ;; end module
