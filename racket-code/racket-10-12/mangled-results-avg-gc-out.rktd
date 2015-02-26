((((naive . xs-left)
   (1 . 3)
   (2 . 5)
   (3 . 2)
   (10 . too-large)
   (20 . too-large)
   (50 . too-large)
   (100 . too-large)
   (200 . too-large))
  ((naive . xs-right)
   (1 . 2)
   (2 . 2)
   (3 . 2)
   (10 . too-large)
   (20 . too-large)
   (50 . too-large)
   (100 . too-large)
   (200 . too-large))
  ((naive . parens)
   (0 . 2)
   (2 . 2)
   (2 . 2)
   (10 . too-large)
   (20 . too-large)
   (50 . too-large)
   (100 . too-large)
   (200 . too-large))
  ((naive . sexp)
   (1 . 2)
   (2 . 2)
   (3 . too-large)
   (8 . too-large)
   (20 . too-large)
   (48 . too-large)
   (100 . too-large)
   (200 . too-large))
  ((naive . add-expr)
   (1 . 2)
   (3 . too-large)
   (3 . too-large)
   (11 . too-large)
   (21 . too-large)
   (51 . too-large)
   (101 . too-large)
   (201 . too-large))
  ((naive . cf-add-expr)
   (1 . 2)
   (1 . 2)
   (3 . too-large)
   (9 . too-large)
   (17 . too-large)
   (49 . too-large)
   (97 . too-large)
   (201 . too-large))
  ((naive . after-parens)
   (1 . 2)
   (3 . too-large)
   (3 . too-large)
   (11 . too-large)
   (21 . too-large)
   (51 . too-large)
   (101 . too-large)
   (201 . too-large))
  ((compaction . xs-left)
   (1 . 2)
   (2 . 3)
   (3 . 3)
   (10 . 4)
   (20 . 6)
   (50 . 12)
   (100 . 23)
   (200 . 50))
  ((compaction . xs-right)
   (1 . 3)
   (2 . 3)
   (3 . 3)
   (10 . 5)
   (20 . 7)
   (50 . 14)
   (100 . 32)
   (200 . 55))
  ((compaction . parens)
   (0 . 3)
   (2 . 4)
   (2 . 4)
   (10 . 9)
   (20 . 30)
   (50 . 157)
   (100 . too-large)
   (200 . too-large))
  ((compaction . sexp)
   (1 . 4)
   (2 . 5)
   (3 . 6)
   (8 . 11)
   (20 . 41)
   (48 . 180)
   (100 . too-large)
   (200 . too-large))
  ((compaction . add-expr)
   (1 . 5)
   (3 . 6)
   (3 . 6)
   (11 . 26)
   (21 . too-large)
   (51 . too-large)
   (101 . too-large)
   (201 . too-large))
  ((compaction . cf-add-expr)
   (1 . 5)
   (1 . 5)
   (3 . 7)
   (9 . 18)
   (17 . 48)
   (49 . 279)
   (97 . too-large)
   (201 . too-large))
  ((compaction . after-parens)
   (1 . 7)
   (3 . 8)
   (3 . 8)
   (11 . 17)
   (21 . 45)
   (51 . 231)
   (101 . too-large)
   (201 . too-large))
  ((zipper . xs-left)
   (1 . 7)
   (2 . 7)
   (3 . 8)
   (10 . 9)
   (20 . 12)
   (50 . 20)
   (100 . 40)
   (200 . 65))
  ((zipper . xs-right)
   (1 . 7)
   (2 . 7)
   (3 . 8)
   (10 . 10)
   (20 . 12)
   (50 . 20)
   (100 . 39)
   (200 . 65))
  ((zipper . parens)
   (0 . 6)
   (2 . 7)
   (2 . 7)
   (10 . 10)
   (20 . 13)
   (50 . 22)
   (100 . 42)
   (200 . 74))
  ((zipper . sexp)
   (1 . 7)
   (2 . 7)
   (3 . 9)
   (8 . 12)
   (20 . 22)
   (48 . 45)
   (100 . 89)
   (200 . 150))
  ((zipper . add-expr)
   (1 . 7)
   (3 . 9)
   (3 . 10)
   (11 . 38)
   (21 . too-large)
   (51 . too-large)
   (101 . too-large)
   (201 . too-large))
  ((zipper . cf-add-expr)
   (1 . 8)
   (1 . 8)
   (3 . 11)
   (9 . 27)
   (17 . 65)
   (49 . 536)
   (97 . too-large)
   (201 . too-large))
  ((zipper . after-parens)
   (1 . 12)
   (3 . 14)
   (3 . 13)
   (11 . 23)
   (21 . 58)
   (51 . 374)
   (101 . too-large)
   (201 . too-large)))
 ((((naive . xs-left) (4 11 24 49 98 too-large))
   ((naive . xs-right) (4 11 24 49 98 too-large))
   ((naive . parens) (6 18 42 90 too-large))
   ((naive . sexp) (9 27 81 too-large))
   ((naive . add-expr) (5 16 50 too-large))
   ((naive . cf-add-expr) (15 48 too-large))
   ((naive . after-parens) (11 31 71 too-large)))
  (((compaction . xs-left)
    (4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4))
   ((compaction . xs-right)
    (4
     5
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6))
   ((compaction . parens)
    (6
     7
     9
     11
     13
     15
     17
     19
     21
     23
     25
     27
     29
     31
     33
     35
     37
     39
     41
     43
     45
     47
     49
     51
     53
     55
     51
     49
     47
     45
     43
     41
     39
     37
     35
     33
     31
     29
     27
     25
     23
     21
     19
     17
     15
     13
     11
     9
     7
     5
     2))
   ((compaction . sexp)
    (9
     10
     13
     14
     17
     18
     21
     22
     25
     26
     29
     30
     33
     34
     37
     38
     41
     42
     45
     46
     49
     50
     53
     54
     57
     58
     55
     55
     51
     51
     47
     47
     43
     43
     39
     39
     35
     35
     31
     31
     27
     27
     23
     23
     19
     19
     15
     15
     2))
   ((compaction . add-expr)
    (5 8 9 11 14 16 22 25 34 37 49 52 67 70 88 91 too-large))
   ((compaction . cf-add-expr)
    (15
     18
     22
     23
     25
     29
     33
     34
     36
     40
     44
     45
     47
     51
     55
     56
     58
     62
     66
     67
     69
     73
     77
     78
     80
     84
     81
     83
     85
     74
     70
     74
     77
     63
     59
     63
     66
     52
     48
     52
     55
     41
     37
     41
     44
     30
     26
     30
     33
     19))
   ((compaction . after-parens)
    (11
     12
     14
     16
     18
     20
     22
     24
     26
     28
     30
     32
     34
     36
     38
     40
     42
     44
     46
     48
     50
     52
     54
     56
     58
     60
     56
     54
     52
     50
     48
     46
     44
     42
     40
     38
     36
     34
     32
     30
     28
     26
     24
     22
     20
     18
     16
     14
     12
     10
     7
     1)))
  (((zipper . xs-left)
    (4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4))
   ((zipper . xs-right)
    (4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4
     4))
   ((zipper . parens)
    (6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     6
     1
     1
     1
     1
     1
     1
     1
     1
     1
     1
     1
     1
     1
     1
     1
     1
     1
     1
     1
     1
     1
     1
     1
     1
     1))
   ((zipper . sexp)
    (9
     9
     11
     9
     11
     9
     11
     9
     11
     9
     11
     9
     11
     9
     11
     9
     11
     9
     11
     9
     11
     9
     11
     9
     11
     12
     12
     12
     12
     12
     12
     12
     12
     12
     12
     12
     12
     12
     12
     12
     12
     12
     12
     12
     12
     12
     12
     12
     1))
   ((zipper . add-expr)
    (5 8 8 11 13 16 22 25 34 37 49 52 67 70 88 91 too-large))
   ((zipper . cf-add-expr)
    (15
     18
     22
     22
     25
     29
     33
     33
     36
     40
     44
     44
     47
     51
     55
     55
     58
     62
     66
     66
     69
     73
     77
     77
     80
     84
     80
     82
     85
     73
     69
     74
     77
     62
     58
     63
     66
     51
     47
     52
     55
     40
     36
     41
     44
     29
     25
     30
     33
     18))
   ((zipper . after-parens)
    (11
     12
     14
     16
     18
     20
     22
     24
     26
     28
     30
     32
     34
     36
     38
     40
     42
     44
     46
     48
     50
     52
     54
     56
     58
     60
     54
     51
     49
     47
     45
     43
     41
     39
     37
     35
     33
     31
     29
     27
     25
     23
     21
     19
     17
     15
     13
     11
     9
     7
     5
     1)))))
