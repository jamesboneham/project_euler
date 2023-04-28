;;; pe_elisp.el --- Solutions to Project Euler problems in elisp -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 James Boneham
;;
;; Author: James Boneham <https://github.com/boneham>
;; Maintainer: James Boneham <james.boneham@physics.ox.ac.uk>
;; Created: March 12, 2023
;; Modified: March 12, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/bonehampe_elisp/
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Solutions to Project Euler problems in elisp
;;
;;; Code:

(require 'cl-lib)

(cl-defun pe_elisp-format-time (time)
  "Select appropriate unit of TIME and format accordingly."
  (cond ((> 0.000001 time) (format "%.3s ns" (* time 1000000000)))
        ((> 0.001 time) (format "%.3s μs" (* time 1000000)))
        ((> 1.0 time) (format "%.3s ms" (* time 1000)))
        (t (format "%.3f s" time))))

(cl-defmacro pe_elisp-timeit (&rest body)
  "Execute BODY and return formatted result and execution time."
  `(let* ((tinit (float-time))
          (res ,@body)
          (tend (float-time))
          (t1 (- tend tinit))
          (n (ceiling (/ 1.0 t1)))
          (tn (if (= n 1) t1
                (car (benchmark-run n ,@body))))
          (tavg (/ tn n)))
     (format "Result:\n\t%s\nAverage execution time (%s calls):\n\t%s"
             res n (pe_elisp-format-time tavg))))

(cl-defun pe_elisp-eratosthenes (nmax)
  "Generate list of primes up to NMAX using the Sieve of Eratosthenes."
 (let ((arr (make-vector (+ nmax 1) t)))
  (cl-loop
   for i = 2 then (+ i 1)
   for i2 = (* i i) until (> i2 nmax) do
   (when (aref arr i)
     (cl-loop
      for j = i2 then (+ j i) until (> j nmax) do
      (aset arr j nil))))
  (cl-loop
   for i = 2 then (+ i 1) until (> i nmax)
   when (aref arr i) collect i)))

(cl-defun pe_elisp001 (&optional (nmax 1000))
  "Find the sum of all the multiples of 3 or 5 below NMAX (default 1000)."
  (let* ((n (- nmax 1)))
    (- (+ (cl-loop for x from 3 to n by 3 sum x)
          (cl-loop for x from 5 to n by 5 sum x))
       (cl-loop for x from 15 to n by 15 sum x))))

(cl-defun pe_elisp002 (&optional (nmax 4000000))
  "Find the sum of the even-valued Fibonacci numbers up to NMAX (default 4000000)."
  (cl-loop for x = 2 then (+ y (* 4 x)) and y = 0 then x
         until (> x nmax) sum x))

(cl-defun pe_elisp003 (&optional (n0 600851475143))
  "Find largest prime factor of N0."
  (cl-loop
   for fac = 3 then (+ fac 2)
   for n = (cl-loop for m = n0 then (/ m 2) while (= 0 (% m 2))
                    finally return m)
   then (cl-loop for m = n then (/ m fac) while (= 0 (% m fac))
                 finally return m)
   until (> fac n)
   finally return fac))

(cl-defun pe_elisp004 ()
  "Find the largest palindrome which is a product of two three digit numbers."
 (cl-labels
    ((digstonum
      (d1d2d3)
      (cl-destructuring-bind (d1 d2d3) (cl-floor d1d2d3 100)
        (cl-destructuring-bind (d2 d3) (cl-floor d2d3 10)
          (+ (* 100001 d1) (* 10010 d2) (* 1100 d3)))))
     (rounduptoodd (n) (if (cl-evenp n) (+ n 1) n))
     (testpaleven
      (n)
      (cl-loop for f from (/ n 999) to (cl-isqrt n)
               if (= 0 (% n f)) return n))
     (testpalodd
      (n)
      (cl-loop for f from (rounduptoodd (/ n 999)) to (cl-isqrt n) by 2
               if (= 0 (% n f)) return n))
     (testpal
      (n)
      (if (cl-evenp n) (testpaleven n) (testpalodd n))))
  (cl-loop for stem = 999 then (- stem 1) until (< stem 100)
           for num = (digstonum stem)
           if (testpal num) return it)))

(cl-defun pe_elisp005 (&optional (nmax 20))
  "Find lowest common multiple of integers up to NMAX."
    (apply #'cl-lcm (cl-loop for x from 2 to nmax collect x)))

(cl-defun pe_elisp006 (&optional (nmax 100))
  "Difference between the squared sum and sum of squares for all integers up to NMAX."
    (cl-loop for i from 1 to nmax
         and s1 = 0 then (+ s1 i)
         and s2 = 0 then (+ s2 (* i i))
         finally return (- (* s1 s1) s2)))

(cl-defun pe_elisp007 (&optional (nmax 10001))
  "Return the NMAX prime number."
  (cl-letf* ((plim (car (cl-truncate (* nmax (log (* nmax (log nmax))))))))
    (nth (- nmax 1) (pe_elisp-eratosthenes plim))))

(cl-defun pe_elisp008 ()
  "Return the maximum 13 adjacent-digit product in following series."
  (cl-letf* ((nums '(7 3 1 6 7 1 7 6 5 3 1 3 3 0 6 2 4 9 1 9 2 2 5 1 1
                       9 6 7 4 4 2 6 5 7 4 7 4 2 3 5 5 3 4 9 1 9 4 9 3 4
                       9 6 9 8 3 5 2 0 3 1 2 7 7 4 5 0 6 3 2 6 2 3 9 5 7
                       8 3 1 8 0 1 6 9 8 4 8 0 1 8 6 9 4 7 8 8 5 1 8 4 3
                       8 5 8 6 1 5 6 0 7 8 9 1 1 2 9 4 9 4 9 5 4 5 9 5 0
                       1 7 3 7 9 5 8 3 3 1 9 5 2 8 5 3 2 0 8 8 0 5 5 1 1
                       1 2 5 4 0 6 9 8 7 4 7 1 5 8 5 2 3 8 6 3 0 5 0 7 1
                       5 6 9 3 2 9 0 9 6 3 2 9 5 2 2 7 4 4 3 0 4 3 5 5 7
                       6 6 8 9 6 6 4 8 9 5 0 4 4 5 2 4 4 5 2 3 1 6 1 7 3
                       1 8 5 6 4 0 3 0 9 8 7 1 1 1 2 1 7 2 2 3 8 3 1 1 3
                       6 2 2 2 9 8 9 3 4 2 3 3 8 0 3 0 8 1 3 5 3 3 6 2 7
                       6 6 1 4 2 8 2 8 0 6 4 4 4 4 8 6 6 4 5 2 3 8 7 4 9
                       3 0 3 5 8 9 0 7 2 9 6 2 9 0 4 9 1 5 6 0 4 4 0 7 7
                       2 3 9 0 7 1 3 8 1 0 5 1 5 8 5 9 3 0 7 9 6 0 8 6 6
                       7 0 1 7 2 4 2 7 1 2 1 8 8 3 9 9 8 7 9 7 9 0 8 7 9
                       2 2 7 4 9 2 1 9 0 1 6 9 9 7 2 0 8 8 8 0 9 3 7 7 6
                       6 5 7 2 7 3 3 3 0 0 1 0 5 3 3 6 7 8 8 1 2 2 0 2 3
                       5 4 2 1 8 0 9 7 5 1 2 5 4 5 4 0 5 9 4 7 5 2 2 4 3
                       5 2 5 8 4 9 0 7 7 1 1 6 7 0 5 5 6 0 1 3 6 0 4 8 3
                       9 5 8 6 4 4 6 7 0 6 3 2 4 4 1 5 7 2 2 1 5 5 3 9 7
                       5 3 6 9 7 8 1 7 9 7 7 8 4 6 1 7 4 0 6 4 9 5 5 1 4
                       9 2 9 0 8 6 2 5 6 9 3 2 1 9 7 8 4 6 8 6 2 2 4 8 2
                       8 3 9 7 2 2 4 1 3 7 5 6 5 7 0 5 6 0 5 7 4 9 0 2 6
                       1 4 0 7 9 7 2 9 6 8 6 5 2 4 1 4 5 3 5 1 0 0 4 7 4
                       8 2 1 6 6 3 7 0 4 8 4 4 0 3 1 9 9 8 9 0 0 0 8 8 9
                       5 2 4 3 4 5 0 6 5 8 5 4 1 2 2 7 5 8 8 6 6 6 8 8 1
                       1 6 4 2 7 1 7 1 4 7 9 9 2 4 4 4 2 9 2 8 2 3 0 8 6
                       3 4 6 5 6 7 4 8 1 3 9 1 9 1 2 3 1 6 2 8 2 4 5 8 6
                       1 7 8 6 6 4 5 8 3 5 9 1 2 4 5 6 6 5 2 9 4 7 6 5 4
                       5 6 8 2 8 4 8 9 1 2 8 8 3 1 4 2 6 0 7 6 9 0 0 4 2
                       2 4 2 1 9 0 2 2 6 7 1 0 5 5 6 2 6 3 2 1 1 1 1 1 0
                       9 3 7 0 5 4 4 2 1 7 5 0 6 9 4 1 6 5 8 9 6 0 4 0 8
                       0 7 1 9 8 4 0 3 8 5 0 9 6 2 4 5 5 4 4 4 3 6 2 9 8
                       1 2 3 0 9 8 7 8 7 9 9 2 7 2 4 4 2 8 4 9 0 9 1 8 8
                       8 4 5 8 0 1 5 6 1 6 6 0 9 7 9 1 9 1 3 3 8 7 5 4 9
                       9 2 0 0 5 2 4 0 6 3 6 8 9 9 1 2 5 6 0 7 1 7 6 0 6
                       0 5 8 8 6 1 1 6 4 6 7 1 0 9 4 0 5 0 7 7 5 4 1 0 0
                       2 2 5 6 9 8 3 1 5 5 2 0 0 0 5 5 9 3 5 7 2 9 7 2 5
                       7 1 6 3 6 2 6 9 5 6 1 8 8 2 6 7 0 4 2 8 2 5 2 4 8
                       3 6 0 0 8 2 3 2 5 7 5 3 0 4 2 0 7 5 2 9 6 3 4 5 0))
             (groups (cl-remove-if (lambda (x) (< (cl-list-length x) 13))
                                   (car
                                    (cl-reduce
                                     (lambda (x y)
                                       (if (= 0 y) `(,(cons (cadr x) (car x)) nil)
                                         `(,(car x) ,(cons y (cadr x)))))
                                     nums :initial-value '(nil nil))))))
    (cl-labels
        ((maxprod
          (nums)
          (cl-loop for x from 0 to (- (cl-list-length nums) 13)
                   and y = 0 then (max y (cl-reduce #'* (cl-subseq nums x (+ x 13))))
                   finally return y)))
      (cl-reduce (lambda (x y) (max x (maxprod y))) groups :initial-value 0))))

(cl-defun pe_elisp009 (&optional (target 1000))
  "Find pythagorean triple with a + b + c = TARGET, and return product abc."
  (cl-loop
   for m from 2 to (cl-isqrt (/ target 2))
   if (cl-loop
       for n from 1 to (min (/ (- target (* 2 m m)) m) (- m 1))
       if (= 0 (% 1000 (* 2 m (+ m n))))
       return (* 2 m n (expt (/ target (* 2 m (+ m n))) 3)
                 (- (expt m 4) (expt n 4))))
   return it))

(cl-defun pe_elisp010 (&optional (nmax 2000000))
  "Sum all primes below NMAX."
  (cl-reduce #'+ (pe_elisp-eratosthenes nmax)))

(cl-defun pe_elisp011 ()
  "Find maximum four adjacent-digit (up, down, or diagonal) product in following grid."
  (cl-letf*
      ((rows '((08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08)
               (49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00)
               (81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65)
               (52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91)
               (22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80)
               (24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50)
               (32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70)
               (67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21)
               (24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72)
               (21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95)
               (78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92)
               (16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57)
               (86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58)
               (19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40)
               (04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66)
               (88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69)
               (04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36)
               (20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16)
               (20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54)
               (01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48)))
       (n (cl-list-length rows))
       (cols (cl-mapcar (lambda (x) (cl-mapcar (lambda (row) (nth x row)) rows))
                        (cl-loop for i from 0 to (- n 1) collect i)))
       (diags1
        (cl-letf* ((inds (cl-loop for i from -19 to 19
                                  collect (if (< i 0) `(,(abs i) 0) `(0 ,i)))))
          (cl-loop for (i j) in inds
                   for kmax = (- 19 (max i j))
                   collect (cl-loop for k from 0 to kmax
                                    collect (nth (+ j k) (nth (+ i k) rows))))))
       (diags2
        (cl-letf* ((inds (cl-loop for i from -19 to 19
                                  collect (if (< i 0) `(,(abs i) 0) `(0 ,i))))
                   (flipped (cl-mapcar #'reverse rows)))
          (cl-loop for (i j) in inds
                   for kmax = (- 19 (max i j))
                   collect (cl-loop for k from 0 to kmax
                                    collect (nth (+ j k) (nth (+ i k) flipped)))))))
    (cl-labels ((filterlen (lst) (cl-remove-if (lambda (x) (< (cl-list-length x) 4)) lst))
                (group
                 (q &optional b g)
                 (if (null q) (filterlen (if (null b) g `(,b . ,g)))
                   (pcase-let* ((`(,hd . ,tl) q))
                     (if (zerop hd) (group tl nil `(,b . ,g))
                       (group tl `(,hd . ,b) g)))))
                (maxprod
                 (g)
                 (let* ((imax (- (cl-list-length g) 1))
                        (prods (list (cl-reduce '* g :end 4))))
                   (cl-loop for i1 from 4 to imax and i0 from 0 to (- imax 4)
                            do (push (/ (* (car prods) (nth i1 g)) (nth i0 g)) prods)
                            finally return (apply #'max prods)))))
      (cl-loop for g in (cl-mapcan #'group (cl-concatenate 'list rows cols diags1 diags2))
               maximize (maxprod g)))))

(cl-defun pe_elisp012 (&optional (ndivs 500) (nmax 15000))
  "Return the first triangular number with at least NDIVS divisors.
Achieves this finding divisors of all numbers up to NMAX using a sieve. If the
sieve is not large enough, and no answer is found, calls itself again with a
sieve twice as large."
  (let* ((nby2 (/ nmax 2))
         (s (make-vector (+ nmax 1) 1)))
    (cl-loop
     for i from 2 to nby2
     when (= (aref s i) 1)
     do (cl-loop
         for p0 = i then (* p0 i) and e = 1 then (+ e 1) until (> p0 nmax)
         do (cl-loop
             for p = p0 then (+ p p0) until (> p nmax) with e1 = (+ e 1)
             do (aset s p (/ (* (aref s p) e1) e)))))
    (cl-loop
     for i from (if (cl-evenp nby2) (+ nby2 1) nby2) to nmax by 2
     when (= (aref s i) 1)
     do (aset s i 2))
    (or
     (cl-loop for (i j k) = '(1 1 3) then `(,(+ i 1) ,k ,(+ k 2)) until (> k nmax)
              for sigma0i = (aref s i)
              if (> (* sigma0i (aref s j)) ndivs) return (* i j)
              else if (> (* sigma0i (aref s k)) ndivs) return (* i k) end)
     (pe_elisp012 ndivs (* nmax 2)))))

(cl-defun pe_elisp013 ()
  "Sum the following 100 50-digit numbers and return the first 10 digits."
  (string-to-number
   (substring
    (int-to-string
     (cl-reduce #'+ '(37107287533902102798797998220837590246510135740250
                      46376937677490009712648124896970078050417018260538
                      74324986199524741059474233309513058123726617309629
                      91942213363574161572522430563301811072406154908250
                      23067588207539346171171980310421047513778063246676
                      89261670696623633820136378418383684178734361726757
                      28112879812849979408065481931592621691275889832738
                      44274228917432520321923589422876796487670272189318
                      47451445736001306439091167216856844588711603153276
                      70386486105843025439939619828917593665686757934951
                      62176457141856560629502157223196586755079324193331
                      64906352462741904929101432445813822663347944758178
                      92575867718337217661963751590579239728245598838407
                      58203565325359399008402633568948830189458628227828
                      80181199384826282014278194139940567587151170094390
                      35398664372827112653829987240784473053190104293586
                      86515506006295864861532075273371959191420517255829
                      71693888707715466499115593487603532921714970056938
                      54370070576826684624621495650076471787294438377604
                      53282654108756828443191190634694037855217779295145
                      36123272525000296071075082563815656710885258350721
                      45876576172410976447339110607218265236877223636045
                      17423706905851860660448207621209813287860733969412
                      81142660418086830619328460811191061556940512689692
                      51934325451728388641918047049293215058642563049483
                      62467221648435076201727918039944693004732956340691
                      15732444386908125794514089057706229429197107928209
                      55037687525678773091862540744969844508330393682126
                      18336384825330154686196124348767681297534375946515
                      80386287592878490201521685554828717201219257766954
                      78182833757993103614740356856449095527097864797581
                      16726320100436897842553539920931837441497806860984
                      48403098129077791799088218795327364475675590848030
                      87086987551392711854517078544161852424320693150332
                      59959406895756536782107074926966537676326235447210
                      69793950679652694742597709739166693763042633987085
                      41052684708299085211399427365734116182760315001271
                      65378607361501080857009149939512557028198746004375
                      35829035317434717326932123578154982629742552737307
                      94953759765105305946966067683156574377167401875275
                      88902802571733229619176668713819931811048770190271
                      25267680276078003013678680992525463401061632866526
                      36270218540497705585629946580636237993140746255962
                      24074486908231174977792365466257246923322810917141
                      91430288197103288597806669760892938638285025333403
                      34413065578016127815921815005561868836468420090470
                      23053081172816430487623791969842487255036638784583
                      11487696932154902810424020138335124462181441773470
                      63783299490636259666498587618221225225512486764533
                      67720186971698544312419572409913959008952310058822
                      95548255300263520781532296796249481641953868218774
                      76085327132285723110424803456124867697064507995236
                      37774242535411291684276865538926205024910326572967
                      23701913275725675285653248258265463092207058596522
                      29798860272258331913126375147341994889534765745501
                      18495701454879288984856827726077713721403798879715
                      38298203783031473527721580348144513491373226651381
                      34829543829199918180278916522431027392251122869539
                      40957953066405232632538044100059654939159879593635
                      29746152185502371307642255121183693803580388584903
                      41698116222072977186158236678424689157993532961922
                      62467957194401269043877107275048102390895523597457
                      23189706772547915061505504953922979530901129967519
                      86188088225875314529584099251203829009407770775672
                      11306739708304724483816533873502340845647058077308
                      82959174767140363198008187129011875491310547126581
                      97623331044818386269515456334926366572897563400500
                      42846280183517070527831839425882145521227251250327
                      55121603546981200581762165212827652751691296897789
                      32238195734329339946437501907836945765883352399886
                      75506164965184775180738168837861091527357929701337
                      62177842752192623401942399639168044983993173312731
                      32924185707147349566916674687634660915035914677504
                      99518671430235219628894890102423325116913619626622
                      73267460800591547471830798392868535206946944540724
                      76841822524674417161514036427982273348055556214818
                      97142617910342598647204516893989422179826088076852
                      87783646182799346313767754307809363333018982642090
                      10848802521674670883215120185883543223812876952786
                      71329612474782464538636993009049310363619763878039
                      62184073572399794223406235393808339651327408011116
                      66627891981488087797941876876144230030984490851411
                      60661826293682836764744779239180335110989069790714
                      85786944089552990653640447425576083659976645795096
                      66024396409905389607120198219976047599490197230297
                      64913982680032973156037120041377903785566085089252
                      16730939319872750275468906903707539413042652315011
                      94809377245048795150954100921645863754710598436791
                      78639167021187492431995700641917969777599028300699
                      15368713711936614952811305876380278410754449733078
                      40789923115535562561142322423255033685442488917353
                      44889911501440648020369068063960672322193204149535
                      41503128880339536053299340368006977710650566631954
                      81234880673210146739058568557934581403627822703280
                      82616570773948327592232845941706525094512325230608
                      22918802058777319719839450180888072429661980811197
                      77158542502016545090413245809786882778948721859617
                      72107838435069186155435662884062257473692284509516
                      20849603980134001723930671666823555245252804609722
                      53503534226472524250874054075591789781264330331690)))
    0 10)))

(cl-defun pe_elisp014 (&optional (nmax 1000000))
  "Return the starting number with the longest Collatz chain below NMAX.
\"max-specpdl-size\" and \"max-lisp-eval-depth\" are both temporarily increased
as the default values result in recursion errors for the value of NMAX required
by the problem"
  (cl-letf* ((nmin (/ nmax 2))
             (a (make-vector (+ nmax 1) nil))
             (max-specpdl-size 3200)
             (max-lisp-eval-depth 2000))
    (aset a 1 1)
    (cl-labels
        ((collatz
          (n)
          (cond ((> n nmax)
                 (+ 1 (collatz (if (cl-evenp n) (/ n 2) (+ (* 3 n) 1)))))
                ((null (aref a n))
                 (aset a n (+ 1 (collatz (if (cl-evenp n) (/ n 2) (+ (* 3 n) 1))))))
                (t (aref a n)))))
      (cl-loop
       for n = nmax then (- n 1) until (< n nmin)
       for (m cmax) = `(,n ,(collatz n))
       then (let ((c (collatz n))) (if (> c cmax) `(,n ,c) `(,m ,cmax)))
       finally return m))))

(cl-defun pe_elisp015 (&optional (n 20))
  "Return number of routes from top left to bottom right corner in N x N grid.
The only allowed moves are either right or down"
  (cl-loop for r from 1 to n
           and row = (make-vector (+ n 1) 1)
           then (cl-loop for i from 1 to n
                         and j from 0 to (- n 1)
                         do (incf (aref row i) (aref row j))
                         finally return row)
           finally return (aref row n)))

(cl-defun pe_elisp016 (&optional (n0 (expt 2 1000)))
  "Sum the digits of N0."
 (cl-loop for n = n0 then (/ n 10) until (= n 0) sum (% n 10)))

(cl-defun pe_elisp017 ()
  "Solves problem 17.
Return the number of letters used if all the numbers from 1 to 1000
inclusive were written out in words."
 (cl-letf ((l1 3) (l2 3) (l3 5) (l4 4) (l5 4) (l6 3) (l7 5) (l8 5) (l9 4)
          (l10 3) (l11 6) (l12 6) (l13 8) (l14 8) (l15 7) (l16 7) (l17 9) (l18 8) (l19 8)
          (l20 6) (l30 6) (l40 5) (l50 5) (l60 5) (l70 7) (l80 6) (l90 6)
          (l100 7) (l1000 8) (land 3))
  (+ (* 90 (+ l1 l2 l3 l4 l5 l6 l7 l8 l9))
     (* 10 (+ l10 l11 l12 l13 l14 l15 l16 l17 l18 l19))
     (* 100 (+ l20 l30 l40 l50 l60 l70 l80 l90))
     (* 100 (+ l1 l2 l3 l4 l5 l6 l7 l8 l9 (* 9 l100)))
     (+ l1 l1000)
     (* 9 99 land))))

(cl-defun pe_elisp018 ()
  "Return max path sum through following triangle."
  (cl-labels ((mapfun (x y) (+ (car y) (max (car x) (cadr x))))
              (reducefun (x y) (cl-maplist #'mapfun x y)))
    (car (cl-reduce #'reducefun '((04 62 98 27 23 09 70 98 73 93 38 53 60 04 23)
                                  (63 66 04 68 89 53 67 30 73 16 69 87 40 31)
                                  (91 71 52 38 17 14 91 43 58 50 27 29 48)
                                  (70 11 33 28 77 73 17 78 39 68 17 57)
                                  (53 71 44 65 25 43 91 52 97 51 14)
                                  (41 48 72 33 47 32 37 16 94 29)
                                  (41 41 26 56 83 40 80 70 33)
                                  (99 65 04 28 06 16 70 92)
                                  (88 02 77 73 07 63 67)
                                  (19 01 23 75 03 34)
                                  (20 04 82 47 65)
                                  (18 35 87 10)
                                  (17 47 82)
                                  (95 64)
                                  (75))))))

(cl-defun pe_elisp019 ()
  "Solves problem 19.
Return the number of Sundays which fell on the 1st of the month in the 21st century."
  (cl-loop
   for time0 = (time-add (encode-time (parse-time-string "Mon 1 Jan 1900 12:00:00"))
                         (* 24 60 60 365))
   then (+ time0 86400)
   for (nil nil nil nil nil nil dow nil nil) = (decode-time time0)
   until (= dow 0)
   finally return (cl-loop
                   for time = time0 then (+ time 604800)
                   for (nil nil nil dom nil yr nil nil nil) = (decode-time time)
                   until (>= yr 2001)
                   count (= dom 1))))

(cl-defun pe_elisp020 (&optional (n0 100))
  "Sum the digits of N0 factorial."
  (cl-loop
   for n = (cl-loop
            for m = n0 then (- m 1) until (zerop m)
            for fac = n0 then (* m fac)
            finally return fac)
   then (/ n 10) until (= n 0) sum (% n 10)))

(cl-defmacro pe_elisp-run (pnum &rest args)
  "Prints the result and execution time of problem PNUM with arguments ARGS."
  `(pe_elisp-timeit (,(intern (format "pe_elisp%03d" pnum)) ,@args)))

(cl-defun pe_elisp-read-pnum ()
  "Interaction loop."
  (cl-letf* ((pnumstr
              (read-string "Enter problem number (or any non-numeric value to quit)\n >>> "))
             (pnum (string-to-number pnumstr)))
    (unless (zerop pnum)
      (if (and (integerp pnum) (>= pnum 1))
          (progn
            (princ (cl-case pnum (1 (pe_elisp-run 1))
                          (2 (pe_elisp-run 2))
                          (3 (pe_elisp-run 3))
                          (4 (pe_elisp-run 4))
                          (5 (pe_elisp-run 5))
                          (6 (pe_elisp-run 6))
                          (7 (pe_elisp-run 7))
                          (8 (pe_elisp-run 8))
                          (9 (pe_elisp-run 9))
                          (10 (pe_elisp-run 10))
                          (11 (pe_elisp-run 11))
                          (12 (pe_elisp-run 12))
                          (13 (pe_elisp-run 13))
                          (14 (pe_elisp-run 14))
                          (15 (pe_elisp-run 15))
                          (16 (pe_elisp-run 16))
                          (17 (pe_elisp-run 17))
                          (18 (pe_elisp-run 18))
                          (19 (pe_elisp-run 19))
                          (20 (pe_elisp-run 20))))
            (princ "\n"))
        (princ "Problem number must be positive integer\n"))
      (pe_elisp-read-pnum))))

(pe_elisp-read-pnum)

(provide 'pe_elisp)
;;; pe_elisp.el ends here
