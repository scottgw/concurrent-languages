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
  end

feature
  live
  do
    --print("worker live%N")
    get_result(matrix)
    put_result(aggregator)
    --print("worker dead%N")
  end

  get_result(a_matrix: separate ARRAY[INTEGER])
  local
    lcg_a, lcg_c, rand_max, int_max: INTEGER
  do
    lcg_a := 1664525
    lcg_c := 1013904223
    rand_max := 100
    int_max := 2147483647
    a_matrix.resize(1, ncols)
    across 1 |..| ncols as jc loop
      seed := (lcg_a * seed + lcg_c) \\ int_max
      a_matrix.put((((seed \\ rand_max) + rand_max) \\ rand_max), jc.item)
    end
  end

  put_result(an_aggregator: separate RANDMAT_PARFOR_AGGREGATOR)
  do
    an_aggregator.put
  end

feature {NONE}
  nrows, ncols, seed: INTEGER
  matrix: separate ARRAY[INTEGER]
  aggregator: separate RANDMAT_PARFOR_AGGREGATOR

end
