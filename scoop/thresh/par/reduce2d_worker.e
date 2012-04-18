class REDUCE2D_WORKER
inherit EXECUTION_ENVIRONMENT
create make
feature
  make (nrows_, ncols_: INTEGER; array_: separate ARRAY[INTEGER])
  local
  do
    nrows := nrows_
    ncols := ncols_
    array := array_
  end

feature
  live
  local
    res: INTEGER
  do

    res := get_result(array)
    -- send result back

    --sleep(100000000 * (index \\ 10))
    --print("!" + index.out + "!")
    --if ((index> 0) and index \\ 10 = 0) then
      --print("%N")
    --end
  end

  get_result(an_array: separate ARRAY[INTEGER]): INTEGER
  local
    j: INTEGER
  do
    Result := an_array.item(1)
    from j := 2 until j > ncols loop
      Result := Result.max(an_array.item(j))
      j := j + 1
    end
  end

feature {NONE}
  nrows, ncols: INTEGER
  array: separate ARRAY[INTEGER]

end
