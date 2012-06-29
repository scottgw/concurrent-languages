-- product: matrix-vector product
--
-- input:
--   matrix: a real matrix
--   vector: a real vector
--   nelts: the number of elements
--
-- output:
--   res: a real vector, whose values are the result of the product

class PRODUCT
inherit ARGUMENTS
create make_empty
feature
  make_empty do end

  put(vector: separate ARRAY[DOUBLE]; value: DOUBLE; index: INTEGER)
  do
    vector.force(value, index)
  end

  create_array(): separate ARRAY[DOUBLE]
  do
    create {separate ARRAY[DOUBLE]} Result.make_empty
  end

  product(nelts: INTEGER; matrix: ARRAY[separate ARRAY[DOUBLE]];
          vector: ARRAY[DOUBLE])
    : ARRAY[DOUBLE]
  do
    -- parallel for on nelts
    Result := parfor(nelts, matrix, vector)
  end

  -- parallel for on nelts
  parfor(nelts: INTEGER; matrix: ARRAY[separate ARRAY[DOUBLE]];
         vector: ARRAY[DOUBLE])
    : ARRAY[DOUBLE]
  local
    res: separate ARRAY[DOUBLE]
    worker: separate PRODUCT_PARFOR_WORKER
    workers: LINKED_LIST[separate PRODUCT_PARFOR_WORKER]
    reader: separate PRODUCT_PARFOR_READER
  do
    res := create_array()
    create workers.make
    create reader.make
    create parfor_aggregator.make(nelts)
    across 1 |..| nelts as ic loop
      create worker.make(nelts, ic.item, matrix.item(ic.item),
        to_separate(nelts, vector), res, parfor_aggregator)
      workers.extend(worker)
    end
    -- parallel for on rows
    workers.do_all(agent launch_parfor_worker)
    parfor_result(reader)
    Result := to_local(nelts, res)
  end

  to_separate(nelts: INTEGER; vector: ARRAY[DOUBLE])
    : separate ARRAY[DOUBLE]
  local
    res: separate ARRAY[DOUBLE]
  do
    create res.make_filled(0.0, 1, nelts)
    across 1 |..| nelts as ic loop
      put(res, vector.item(ic.item), ic.item)
    end
    Result := res
  end

  to_local(nelts: INTEGER; vector: separate ARRAY[DOUBLE])
    : ARRAY[DOUBLE]
  local
    res: ARRAY[DOUBLE]
  do
    create res.make_empty
    across 1 |..| nelts as ic loop
      res.force(vector.item(ic.item), ic.item);
    end
    Result := res
  end

  launch_parfor_worker(worker: separate PRODUCT_PARFOR_WORKER)
  do
    worker.live
  end

  parfor_result(reader: separate PRODUCT_PARFOR_READER)
  do
    reader.get_result(parfor_aggregator)
  end


feature {NONE}
  parfor_aggregator: PRODUCT_PARFOR_AGGREGATOR

end -- class PRODUCT

