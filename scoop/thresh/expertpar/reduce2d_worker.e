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
        accum_: separate CELL [INTEGER]
        op_: INTEGER;
        value_: INTEGER
        )
    do
      start := start_
      final := final_
      ncols := ncols_
      array := array_
      accum := accum_
      op := op_
      value := value_
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
          Result [to_local_row (i), j] := a_sep_array [i, j]
          j := j + 1
        end
        i := i + 1
      end
    end
  
  get_result(a_array: ARRAY2[INTEGER])
    local
      i, j: INTEGER
      res: INTEGER
    do
      inspect op
      when {REDUCE2D_OPERATOR}.max then
        res := {INTEGER_32}.Min_value
      when {REDUCE2D_OPERATOR}.filter then
        res := 0
      end

      from i := start
      until i > final
      loop
        from j := 1
        until j > ncols
        loop
          inspect op
          when {REDUCE2D_OPERATOR}.max then
            res := res.max(a_array [i, j])
          when {REDUCE2D_OPERATOR}.filter then
            res := res + filter(a_array [i, j])
          end
          j := j + 1
        end
        i := i + 1
      end

      update_separate_accumulator (res, accum)
    end

  update_separate_accumulator (res: INTEGER; acc: separate CELL [INTEGER])
    local
      x: INTEGER
    do
      x := acc.item
      inspect op
      when {REDUCE2D_OPERATOR}.max then
        x := res.max(x)
      when {REDUCE2D_OPERATOR}.filter then
        x := res + filter(x)
      end
      acc.put (x)
    end
  
  filter(x: INTEGER): INTEGER
    do
      Result := 0
      if x = value then
        Result := 1
      end
    end

feature {NONE}
  accum: separate CELL [INTEGER]
  start, final: INTEGER
  ncols: INTEGER
  array: separate ARRAY2[INTEGER]
  op: INTEGER
  value: INTEGER

end
