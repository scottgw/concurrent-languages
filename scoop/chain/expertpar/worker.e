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
      create mask.make (start, final)
    end

feature -- Attributes
  start, final: INTEGER
  matrix: ARRAY2 [INTEGER]

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
  seed: INTEGER

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

      acc.put (newmax, 1)

      from i := 0
      until i > 100
      loop
      	h := sep_hist.item (i)
      	sep_hist.put (h + hist [i], i)
        i := i + 1
      end
    end

  live_thresh_map (threshold: INTEGER)
    local
      i, j: INTEGER
    do
      from i := 1
      until i > nelts
      loop
        from j := 1
        until j > nelts
        loop
          if matrix [i, j] >= threshold then
            mask [i, j] = 1
          end
          j := j + 1
        end
        i := i + 1
      end
    end

feature {NONE}
  mask: ARRAY2 [INTEGER]
  histogram: separate ARRAY [INTEGER]
  accum: separate ARRAY [INTEGER]

feature -- Winnowing procedure
  
  live_winnow
    local
      vector: ARRAYED_LIST [TUPLE[INTEGER, INTEGER, INTEGER]]
      i, j: INTEGER
      count: INTEGER
    do
      create vector.make_empty

      from i := start
      until i > final
      loop
        from j := 1
        until j > ncols
        loop
          if mask [i, j] = 1 then
            vector.extend ([matrix [i, j], i, j])
          end
          j := j + 1
        end
        i := i + 1
      end

      put_vectors (vector, v_vector, x_vector, y_vector)
    end

  put_vector (a_vector: ARRAY [TUPLE[v,x,y: INTEGER]];
              vs, xs, ys: separate ARRAY [INTEGER])
    local
      i: INTEGER
      n: INTEGER
      t: TUPLE [v,x,y: INTEGER]
    do
      n := xs.count
      from i := 1
      until i > a_vector.count
      loop
        t := a_vector [i]
        vs [n + i] := t.v
        xs [n + i] := t.x
        ys [n + i] := t.y
        i := i + 1
      end
    end

feature {NONE} -- Winnow attributes
  v_vector, x_vector, y_vector: separate ARRAY [INTEGER]

end

