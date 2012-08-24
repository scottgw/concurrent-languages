class PARFOR_WORKER

create make

feature
  make (start_, final_, ncols_: INTEGER;
        from_array_: separate ARRAY2[INTEGER];
        shared_: separate ARRAY2[INTEGER];
        threshold_: INTEGER)
    do
      start := start_
      final := final_
      ncols := ncols_
      from_array := from_array_
      shared := shared_
      threshold := threshold_
    end

feature
  live
    do
      tag ("live")
      if final >= start then
        get_result(fetch_array (from_array))
      end
    end

  to_local_row (x: INTEGER) : INTEGER
    do
      Result := x - start + 1
    end

  fetch_array (a_sep_array: separate ARRAY2[INTEGER]): ARRAY2 [INTEGER]
    local
      i, j: INTEGER
    do
      tag ("fetch start")
      create Result.make (final - start + 1, ncols)

      from i := start
      until i > final
      loop
        from j := 1
        until j > ncols
        loop
          Result [to_local_row (i), j] := a_sep_array [i, j]
          j := j + 1
        end
        i := i + 1
      end
      tag ("fetch end")
    end

  get_result(a_from_array: ARRAY2[INTEGER])
    local
      i, j: INTEGER
      res: INTEGER
      to_array: ARRAY2 [INTEGER]
    do
      tag ("get_result start")
      create to_array.make (final - start + 1, ncols)

      from i := start
      until i > final
      loop
        from j := 1
        until j > ncols
        loop
          if a_from_array [to_local_row (i), j] >= threshold then
            to_array [to_local_row(i), j] := 1
          else
            to_array [to_local_row (i), j] := 0
          end
          j := j + 1
        end
        i := i + 1
      end

      update_separate_result (to_array, shared)
      tag ("get_result end")
    end

  tag (str: STRING)
    do
      print ("parfor -> " + str + ": (" + start.out + ", " + final.out + ")%N")
    end


  update_separate_result (a_array: ARRAY2 [INTEGER];
                          a_shared: separate ARRAY2 [INTEGER])
    local
      i, j: INTEGER
    do
      from i := start
      until i > final
      loop
        from j := 1
        until j > ncols
        loop
          a_shared.item (i, j).generator.do_nothing
          a_shared.put (a_array [to_local_row (i), j], i ,j)
          j := j + 1
        end
        i := i + 1
      end
    end

feature {NONE}
  start, final: INTEGER
  ncols: INTEGER
  shared: separate ARRAY2[INTEGER]
  from_array: separate ARRAY2[INTEGER]
  threshold: INTEGER

end
