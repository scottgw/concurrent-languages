class PARFOR_WORKER
create make
feature
  make (nrows_, ncols_: INTEGER;
      from_array_, to_array_: separate ARRAY[INTEGER];
      threshold_: INTEGER;
      aggregator_: separate PARFOR_AGGREGATOR)
  local
  do
    nrows := nrows_
    ncols := ncols_
    from_array := from_array_
    to_array := to_array_
    threshold := threshold_
    aggregator := aggregator_
  end

feature
  live
  do
    get_result(from_array, to_array)
  end

  get_result(a_from_array, a_to_array: separate ARRAY[INTEGER])
  do
    across 1 |..| ncols as jc loop
      if a_from_array.item(jc.item) >= threshold then
        a_to_array.force(1, jc.item)
      else
        a_to_array.force(0, jc.item)
      end
    end
    put_result(aggregator)
  end

  put_result(an_aggregator: separate PARFOR_AGGREGATOR)
  do
    an_aggregator.put
  end

feature {NONE}
  nrows, ncols: INTEGER
  from_array, to_array: separate ARRAY[INTEGER]
  threshold: INTEGER
  aggregator: separate PARFOR_AGGREGATOR

end
