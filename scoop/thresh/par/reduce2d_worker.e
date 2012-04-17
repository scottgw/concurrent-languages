class REDUCE2D_WORKER
inherit EXECUTION_ENVIRONMENT
create make
feature
  make (nrows_, ncols_: INTEGER; matrix_: separate ARRAY2[INTEGER];
      index_: INTEGER)
  local
  do
    nrows := nrows_
    ncols := ncols_
    matrix := matrix_
    index := index_
  end

feature
  live
  local
    res: INTEGER
  do

    res := get_result(matrix)
    -- send result back

    --sleep(100000000 * (index \\ 10))
    --print("!" + index.out + "!")
    --if ((index> 0) and index \\ 10 = 0) then
      --print("%N")
    --end
  end

  get_result(a_matrix: separate ARRAY2[INTEGER]): INTEGER
  local
    j: INTEGER
  do
    Result := a_matrix.item(index, 1)
    from j := 2 until j > ncols loop
      Result := Result.max(a_matrix.item(index, j))
      j := j + 1
    end
  end

feature {NONE}
  nrows, ncols: INTEGER
  matrix: separate ARRAY2[INTEGER]
  index: INTEGER

end
