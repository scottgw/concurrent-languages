class REDUCE2D_WORKER
inherit EXECUTION_ENVIRONMENT
create make
feature
  make (nrows_, ncols_: INTEGER; array_: separate ARRAY[INTEGER];
      aggregator_: separate REDUCE2D_AGGREGATOR)
  local
  do
    nrows := nrows_
    ncols := ncols_
    array := array_
    aggregator := aggregator_
  end

feature
  live
  local
    res: INTEGER
  do

    get_result(array, aggregator)

    -- send result back

    --sleep(100000000 * (index \\ 10))
    --print("!" + index.out + "!")
    --if ((index> 0) and index \\ 10 = 0) then
      --print("%N")
    --end
  end

  get_result(an_array: separate ARRAY[INTEGER];
      an_aggregator: separate REDUCE2D_AGGREGATOR)
  local
    j: INTEGER
    res: INTEGER
  do
    res := an_array.item(1)
    if ncols > 1 then
      from j := 2 until j > ncols loop
        res := res.max(an_array.item(j))
        j := j + 1
      end
    end
    print("res: " + res.out + "%N")
    an_aggregator.put(res)
  end

feature {NONE}
  nrows, ncols: INTEGER
  array: separate ARRAY[INTEGER]
  aggregator: separate REDUCE2D_AGGREGATOR

end
