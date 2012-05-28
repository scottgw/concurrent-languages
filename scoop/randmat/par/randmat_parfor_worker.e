class RANDMAT_PARFOR_WORKER
create make
feature
  make (nrows_, ncols_, seed_: INTEGER;
      matrix_: separate ARRAY[INTEGER];
      aggregator_: separate RANDMAT_PARFOR_AGGREGATOR)
  local
  do
    --print("worker created%N")
    nrows := nrows_
    ncols := ncols_
    seed := seed_
    matrix := matrix_
    aggregator := aggregator_
    create rand.set_seed(seed);
  end

feature
  live
  do
    --print("worker live%N")
    get_result(matrix)
    put_result(aggregator)
  end

  get_result(a_matrix: separate ARRAY[INTEGER])
  do
    across 1 |..| ncols as jc loop
      rand.forth
      a_matrix.force(rand.item, jc.item)
    end
    --put_result(aggregator)
  end

  put_result(an_aggregator: separate RANDMAT_PARFOR_AGGREGATOR)
  do
    an_aggregator.put
  end

feature {NONE}
  nrows, ncols, seed: INTEGER
  matrix: separate ARRAY[INTEGER]
  aggregator: separate RANDMAT_PARFOR_AGGREGATOR
	rand: RANDOM

end
