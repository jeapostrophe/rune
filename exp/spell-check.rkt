(on-file-change
 (for ([w (in-words)])
   (cond
     [(spell-check w)
      => (λ (corrections)
           (overlay! w 'spelled-wrong? #t)
           (overlay! w 'spelling-corrections corrections))])))
