class REDUCE2D_WORKER
inherit EXECUTION_ENVIRONMENT
create make
feature
  --make (nrows_, ncols_: INTEGER; matrix_: separate ARRAY2[INTEGER];
      --index_: INTEGER)
  make(index_: INTEGER)
  local
  do
    --nrows := nrows_
    --ncols := ncols_
    --matrix := matrix_
    index := index_
  end

feature
  live
  do
    sleep(100000000 * (index \\ 10))
    print("!" + index.out + "!")
    if ((index> 0) and index \\ 10 = 0) then
      print("%N")
    end
  end

feature {NONE}
  --nrows, ncols: INTEGER
  --matrix: separate ARRAY2[INTEGER]
  index: INTEGER

end
