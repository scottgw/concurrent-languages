class RANDMAT_PARFOR_WORKER
create make
feature
  make (nrows_, ncols_, seed_: INTEGER;
      matrix_: separate ARRAY[INTEGER];
      aggregator_: separate RANDMAT_PARFOR_AGGREGATOR)
  local
  do
    nrows := nrows_
    ncols := ncols_
    seed := seed_.to_natural_32 - 1
    matrix := matrix_
    aggregator := aggregator_
  end

feature
  live
  do
    get_result(matrix)
    put_result(aggregator)
    --print("worker dead%N")
  end

  get_result(a_matrix: separate ARRAY[INTEGER])
  local
    lcg_a, lcg_c, rand_max: NATURAL
  do
    lcg_a := 1664525
    lcg_c := 1013904223
    rand_max := 100
    a_matrix.resize(1, ncols)
    across 1 |..| ncols as jc loop
      seed := lcg_a * seed + lcg_c
      a_matrix.put((seed \\ rand_max).to_integer_32, jc.item)
    end
  end

  put_result(an_aggregator: separate RANDMAT_PARFOR_AGGREGATOR)
  do
    an_aggregator.put
  end

feature {NONE}
  nrows, ncols: INTEGER
  seed: NATURAL
  matrix: separate ARRAY[INTEGER]
  aggregator: separate RANDMAT_PARFOR_AGGREGATOR

end
