(module cpuman-app ()

(import scheme chicken)
(use srfi-1 srfi-13 extras files data-structures)
(use matchable cpuman)


(define (format-frequency frequency) ;; kernel prints frequencies in KHz
  (cond ((and (>= frequency 1000) (< frequency 1000000))
         (conc (/ frequency 1000) "MHz"))
        (else
         (conc (/ frequency 1000000) "GHz"))))

(define (show-cpus proc #!key all-cpus?)
  (for-each (lambda (cpu)
              (print cpu "\t" (proc cpu)))
            (if all-cpus?
                (cpus-available)
                (cpus-online))))

(define (show-cpus-freq frequency-getter)
  (show-cpus (lambda (cpu)
               (format-frequency (frequency-getter cpu)))))
  
(define (cpus-summary)
  (define (show-frequency frequency-getter cpu online?)
    (string-pad-right (if online?
                          (format-frequency (frequency-getter cpu))
                          "--")
                      8))  
  (print
   (string-intersperse
    '("CPU" "Online?" "Governor" "Frequency" "Min Frequency" "Max Frequency")
    "\t"))
  (show-cpus
   (lambda (cpu)
     (let ((online? (cpu-online? cpu)))
       (sprintf "~a\t~a\t~a\t~a\t~a"
                (string-pad-right (if online? "yes" "no") 7)
                (string-pad-right (if online? (cpu-governor cpu) "--") 15)
                (show-frequency cpu-frequency cpu online?)
                (show-frequency cpu-min-frequency cpu online?)
                (show-frequency cpu-max-frequency cpu online?))))
   all-cpus?: #t))


(define (usage #!optional exit-code)
  (let ((port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port)))
        (this (pathname-strip-directory (program-name))))
    (fprintf port #<#EOF
Usage: #this [ <command> ] [ args ]

Available commands:

available                      List available CPUs
online [<set>]                 List/set online CPUs
offline [<set>]                List/set offline CPUs
freq [<set> <freq>]            List/set CPUs' frequency (<freq> in kilo Hertz)
governor [<set> <governor>]    List/set CPUs' scaling governor
max-freq                       List maximum CPUs' frequency
min-freq                       List minimum CPUs' frequency

<set> represents the set of CPUs to apply operations to.  Commas
separate CPU numbers and dashes specify a range.  Example:

    #this online 1,2,4-6

sets CPUs 1, 2, 4, 5 and 6 online.

When no command is provided, #this prints a summary of the available
CPUs.

EOF
)
    (when exit-code (exit exit-code))))

(define (die! fmt . args)
  (apply fprintf (cons (current-error-port) (cons fmt args)))
  (exit 1))
          
(define (check-set given-set)
  (let* ((set (parse-set given-set))
         (available (cpus-available))
         (max-cpus (apply max available)))
    (cond ((memq #f set)
           (die! "Invalid set syntax: ~a\n" given-set))
          ((any (lambda (cpu)
                  (> cpu max-cpus))
                set)
           (die! "One or more CPUs in the prvided set are not available.\nThe available CPUs are: ~a\n"
                 (string-intersperse (map number->string available) ", ")))
          (else set))))

(define (check-governor governor)
  (if (valid-governor? governor)
      governor
      (die! "Invalid governor: ~a" governor)))

(match (command-line-arguments)
  ((or ("--help") ("-help") ("-h")) (usage 0))
  (()              (cpus-summary))
  (("available")   (for-each print (cpus-available)))
  (("online")      (for-each print (cpus-online)))
  (("online" seq)  (set-cpus-online! (check-set seq)))
  (("offline")     (for-each print (cpus-offline)))
  (("offline" seq) (set-cpus-offline! (check-set seq)))
  (("governor")    (show-cpus cpu-governor))
  (("governor" seq governor) (set-cpus-governor! (check-set seq)
                                                 (check-governor governor)))
  (("freq")        (show-cpus-freq cpu-frequency))
  (("freq" seq freq) (set-cpus-frequency! (check-set seq) freq))
  (("max-freq")    (show-cpus-freq cpu-max-frequency))
  (("min-freq")    (show-cpus-freq cpu-min-frequency))
  (else (usage 1)))

) ;; end module
