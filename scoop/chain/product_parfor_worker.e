class PRODUCT_PARFOR_WORKER
create make
feature
  make (nelts_, row_: INTEGER;
      matrix_: separate ARRAY[DOUBLE];
      vector_: separate ARRAY[DOUBLE];
      res_: separate ARRAY[DOUBLE];
      aggregator_: separate PRODUCT_PARFOR_AGGREGATOR)
  local
  do
    nelts := nelts_
    row := row_
    matrix := matrix_
    vector := vector_
    aggregator := aggregator_
    res := res_
  end

feature
  live
  do
    get_result(matrix, vector)
  end

  get_result(a_matrix: separate ARRAY[DOUBLE];
    a_vector: separate ARRAY[DOUBLE])
  local
    sum: DOUBLE
  do
    sum := 0
    across 1 |..| nelts as jc loop
      sum := sum + a_matrix.item(jc.item) * a_vector.item(jc.item);
    end
    put(res, sum, row)
    done(aggregator)
  end

  put(a_vector: separate ARRAY[DOUBLE]; value: DOUBLE; index: INTEGER)
  do
    a_vector.force(value, index);
  end

  done(an_aggregator: separate PRODUCT_PARFOR_AGGREGATOR)
  do
    an_aggregator.done
  end

feature {NONE}
  nelts, row: INTEGER
  matrix, vector, res: separate ARRAY[DOUBLE]
  aggregator: separate PRODUCT_PARFOR_AGGREGATOR

end
