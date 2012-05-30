-- randmat: random number generation
--
-- input:
--   nrows, ncols: the number of rows and cols
--   s: the seed
--
-- output:
--   matrix: a nrows by ncols integer matrix

class RANDMAT
create make_empty
feature
  make_empty do end

  randmat(nrows, ncols, s: INTEGER; matrix: ARRAY[separate ARRAY[INTEGER]])
  do
    -- parallel for on matrix
    parfor(nrows, ncols, s, matrix)
  end

  -- parallel for on matrix
  parfor(nrows, ncols, seed: INTEGER;
    matrix: ARRAY[separate ARRAY[INTEGER]])
  local
    worker: separate RANDMAT_PARFOR_WORKER
    workers: LINKED_LIST[separate RANDMAT_PARFOR_WORKER]
  do
    create workers.make
    create parfor_aggregator.make(nrows)
    across 1 |..| nrows as ic loop
      matrix.force(create_array(), ic.item)
      create worker.make(nrows, ncols, seed, matrix.item(ic.item),
          parfor_aggregator)
      workers.extend(worker)
    end
    -- parallel for on rows
    workers.do_all(agent launch_parfor_worker)
    parfor_result(parfor_aggregator)
  end

  parfor_result(aggregator: separate RANDMAT_PARFOR_AGGREGATOR)
  require
    aggregator.is_all_done
  do
  end

  launch_parfor_worker(worker: separate RANDMAT_PARFOR_WORKER)
  do
    worker.live
  end

  create_array(): separate ARRAY[INTEGER]
  do
    create {separate ARRAY[INTEGER]} Result.make_empty
  end

feature {NONE}
  parfor_aggregator: separate RANDMAT_PARFOR_AGGREGATOR

end -- class RANDMAT 

