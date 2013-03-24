(module cpuman-app ()

(import scheme chicken)
(use srfi-1 srfi-13 extras files data-structures)
(use matchable cpuman)

(declare (uses chicken-syntax))

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
       #this @<profile>

Available commands:

available
  List available CPUs

online [<set>]
  List/set online CPUs

offline [<set>]
  List/set offline CPUs

freq [<freq> [<set>]]
  List/set CPUs' frequency (<freq> in kilo Hertz). If given no argument, list
  CPUs/frequencies. If <set> is not provided, set all online CPUs frequency
  to <freq>.

governor [<governor> [<set>]]
  List/set CPUs' scaling governor.  If given no argument, list CPUs/governors.
  If <set> is not provided, set all online CPUs governor to <governor>.

max-freq
  List maximum CPUs' frequency

min-freq
  List minimum CPUs' frequency

<set> represents the set of CPUs to apply operations to.  Commas
separate CPU numbers and dashes specify a range.  Example:

    #this online 1,2,4-6

sets CPUs 1, 2, 4, 5 and 6 online.

<profile> is either an absolute pathname or a file under ~~/.cpuman or
/etc/cpuman that is loaded by #{this}.  It can contain arbitrary Scheme
code.  This feature can be useful to program custom configuration
using the API provided by the cpuman module.

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

(define (maybe-load-profile)
  (let ((arg (car (command-line-arguments))))
    (and (eq? (string-ref arg 0) #\@)
         (let* ((filename (substring arg 1))
                (profiles
                 (if (absolute-pathname? filename)
                     filename
                     (map (lambda (dir)
                            (make-pathname dir filename))
                          (list "/etc/cpuman"
                                (list (get-environment-variable "HOME")
                                      ".cpuman")))))
                (existing-profiles (and (pair? profiles)
                                        (filter file-exists? profiles))))
           (if (pair? profiles)
               (if (null? existing-profiles)
                   (die! "Could not open profile files.  Attempted\n~a.\nAborting.\n"
                         (string-intersperse profiles "\n"))
                   (for-each load existing-profiles))
               (if (file-exists? profiles) ;; absolute pathname given
                   (load profiles)
                   (die! "Could not open ~a.  Aborting.\n" profiles)))))))

(match (command-line-arguments)
  ((or ("--help") ("-help") ("-h")) (usage 0))
  (()              (cpus-summary))
  (("available")   (for-each print (cpus-available)))
  (("online")      (for-each print (cpus-online)))
  (("online" seq)  (set-cpus-online! (check-set seq)))
  (("offline")     (for-each print (cpus-offline)))
  (("offline" seq) (set-cpus-offline! (check-set seq)))
  (("governor")    (show-cpus cpu-governor))
  (("governor" governor) (set-cpus-governor! (cpus-online)
                                             (check-governor governor)))
  (("governor" governor seq) (set-cpus-governor! (check-set seq)
                                                 (check-governor governor)))
  (("freq")        (show-cpus-freq cpu-frequency))
  (("freq" freq)   (set-cpus-frequency! (cpus-online) freq))
  (("freq" freq seq) (set-cpus-frequency! (check-set seq) freq))
  (("max-freq")    (show-cpus-freq cpu-max-frequency))
  (("min-freq")    (show-cpus-freq cpu-min-frequency))
  (else (or (maybe-load-profile)
            (usage 1))))

) ;; end module
