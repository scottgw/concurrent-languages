class
    WORKER

create
  make

feature {NONE}
  make (start_, final_, nelts_, seed_, percent_, winnow_nelts_: INTEGER)
    do
      start := start_
      final := final_
      nelts := nelts_
      seed  := seed_
      percent := percent_
      winnow_nelts := winnow_nelts_

      create matrix.make (start, final)
    end

feature -- Random matrix generation
  live_randmat
    local
      s, lcg_a, lcg_c, rand_max: NATURAL
      i, j: INTEGER
    do
      lcg_a := 1664525
      lcg_c := 1013904223
      rand_max := 100

      from i := start
      until i > final
      loop
        s := seed + i.to_natural_32 - 1
        from j := 1
        until j > nelts
        loop
          s := lcg_a * s + lcg_c
          matrix [i, j] := (s \\ rand_max).to_integer_32
          j := j + 1
        end
        i := i + 1
      end
    end

feature {NONE}
  matrix: ARRAY2 [INTEGER]

feature -- Thresholding computations
  live_thresh_reduce
    local
      i, j: INTEGER
      max: INTEGER
      hist: ARRAY [INTEGER]
      e: INTEGER
    do
      create hist.make_filled (0, 0, 100)
      max := 0

      from i := start
      until i > final
      loop
        from j := 1
        until j > ncols
        loop
          e        := a_array [i, j]
          hist [e] := hist [e] + 1
          max      := e.max (max)

          j := j + 1
        end
        i := i + 1
      end
      update_histogram (max, accum, hist, histogram)
    end

  update_histogram (max: INTEGER;
                               acc: separate ARRAY [INTEGER];
                               hist: ARRAY [INTEGER];
                               sep_hist: separate ARRAY [INTEGER])
    require
      acc.generator /= Void and sep_hist.generator /= Void
    local
      i: INTEGER
      h: INTEGER
      newmax: INTEGER
    do
      i := acc.item (1)
      newmax := i.max (max)

      if newmax > 100 then
      	(1 / (i-i)).do_nothing
      end
      acc.put (newmax, 1)

      from i := 0
      until i > 100
      loop
      	h := sep_hist.item (i)
      	sep_hist.put (h + hist [i], i)
        i := i + 1
      end
    end

  start, final: INTEGER

feature {NONE}
  histogram: separate ARRAY [INTEGER]
  accum: separate ARRAY [INTEGER]


end

