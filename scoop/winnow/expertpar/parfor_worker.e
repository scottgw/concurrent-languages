class PARFOR_WORKER
create make
feature
  make (nrows_, ncols_, row_: INTEGER;
      matrix_array_, mask_array_: separate ARRAY[INTEGER];
      aggregator_: separate PARFOR_AGGREGATOR)
  local
  do
    nrows := nrows_
    ncols := ncols_
    row := row_
    matrix_array := matrix_array_
    mask_array := mask_array_
    aggregator := aggregator_
  end

feature
  live
  do
    get_result(matrix_array, mask_array)
  end

  get_result(a_matrix_array, a_mask_array: separate ARRAY[INTEGER])
  do
    across 1 |..| ncols as jc loop
      if a_mask_array.item(jc.item) = 1 then
        put(aggregator, [a_matrix_array.item(jc.item), row, jc.item])
      end
    end
    done(aggregator)
  end

  put(an_aggregator: separate PARFOR_AGGREGATOR;
      value: separate TUPLE[INTEGER, INTEGER, INTEGER])
  do
    an_aggregator.put(value)
  end

  done(an_aggregator: separate PARFOR_AGGREGATOR)
  do
    an_aggregator.done
  end

feature {NONE}
  nrows, ncols, row: INTEGER
  matrix_array, mask_array: separate ARRAY[INTEGER]
  aggregator: separate PARFOR_AGGREGATOR

end
