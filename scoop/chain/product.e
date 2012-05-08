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
create make
feature
  make
  local
    nelts: INTEGER
    matrix: ARRAY[separate ARRAY[DOUBLE]]
    vector: ARRAY[DOUBLE]
    res: ARRAY[DOUBLE]
  do
    create in.make_open_read(separate_character_option_value('i'))

    in.read_integer
    nelts := in.last_integer

    create matrix.make_empty
    across 1 |..| nelts as ic loop
      matrix.force(create_array(), ic.item)
      across 1 |..| nelts as jc loop
        put(matrix.item(ic.item), read_double(), jc.item)
      end
    end

    create vector.make_filled(0.0, 1, nelts)
    across 1 |..| nelts as ic loop
      vector.put(read_double(), ic.item)
    end

    res := product(nelts, matrix, vector)

    print(nelts.out + "%N");
    across 1 |..| nelts as ic loop
      print(res.item(ic.item).out + " ");
    end
    print("%N");
  end

  put(vector: separate ARRAY[DOUBLE]; value: DOUBLE; index: INTEGER)
  do
    vector.force(value, index)
  end

  create_array(): separate ARRAY[DOUBLE]
  do
    create {separate ARRAY[DOUBLE]} Result.make_empty
  end

  read_double(): DOUBLE
  do
    in.read_double
    Result := in.last_double
  end

  product(nelts: INTEGER; matrix: ARRAY[separate ARRAY[DOUBLE]];
          vector: ARRAY[DOUBLE])
    : ARRAY[DOUBLE]
  do
    Result := parfor(nelts, matrix, vector)
  end

  -- parallel for on nelts
  parfor(nelts: INTEGER; matrix: ARRAY[separate ARRAY[DOUBLE]];
         vector: ARRAY[DOUBLE])
    : ARRAY[DOUBLE]
  local
    res: separate ARRAY[DOUBLE]
    worker: separate PARFOR_WORKER
    workers: LINKED_LIST[separate PARFOR_WORKER]
    reader: separate PARFOR_READER
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
    create res.make_empty
    across 1 |..| nelts as ic loop
      res.force(vector.item(ic.item), ic.item);
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

  launch_parfor_worker(worker: separate PARFOR_WORKER)
  do
    worker.live
  end

  parfor_result(reader: separate PARFOR_READER)
  do
    reader.get_result(parfor_aggregator)
  end


feature {NONE}
  in: PLAIN_TEXT_FILE
  parfor_aggregator: PARFOR_AGGREGATOR

end -- class PRODUCT

