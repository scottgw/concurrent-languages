class REDUCE2D_WORKER

inherit
  EXECUTION_ENVIRONMENT

create
  make_with_filter

feature
  make_with_filter
       (start_, final_: INTEGER;
        ncols_: INTEGER;
        array_: separate ARRAY2[INTEGER];
        accum_: separate CELL [INTEGER];
        histogram_: separate ARRAY[INTEGER]
        )
    do
      start := start_
      final := final_
      ncols := ncols_
      array := array_
      accum := accum_
      histogram := histogram_
    end

feature
  live
    do
      if start /= final + 1 then
        get_result(fetch_array (array))
      end
    end

  to_local_row (x: INTEGER) : INTEGER
    do
      Result := x - start + 1
    end
  
  fetch_array (a_sep_array: separate ARRAY2[INTEGER]): ARRAY2 [INTEGER]
    require
      a_sep_array.generator /= Void
    local
      i, j: INTEGER
    do
      create Result.make (final - start + 1, ncols)
      
      from i := start
      until i > final
      loop
        from j := 1
        until j > ncols
        loop
          Result [to_local_row (i), j] := a_sep_array.item (i, j)
          j := j + 1
        end
        i := i + 1
      end
    end
  
  get_result(a_array: ARRAY2[INTEGER])
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
          e        := a_array [to_local_row (i), j]
          hist [e] := hist [e] + 1
          max      := e.max (max)
          
          j := j + 1
        end
        i := i + 1
      end
      update_separate_accumulator (max, accum, hist, histogram)
    end

  update_separate_accumulator (max: INTEGER;
                               acc: separate CELL [INTEGER];
                               hist: ARRAY [INTEGER];
                               sep_hist: separate ARRAY [INTEGER])
    local
      i: INTEGER
    do
      i := acc.item
      acc.put (i.max (max))

      from i := 0
      until i > 100
      loop
        sep_hist [i] := sep_hist [i] + hist [i]
        i := i + 1
      end
    end
  
  start, final: INTEGER
  
feature {NONE}
  histogram: separate ARRAY[INTEGER]
  accum: separate CELL [INTEGER]
  
  ncols: INTEGER
  array: separate ARRAY2[INTEGER]

end
