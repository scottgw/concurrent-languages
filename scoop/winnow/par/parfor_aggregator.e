class PARFOR_AGGREGATOR
create make
feature
  make (n_: INTEGER)
  local
  do
    n := n_
    count := 0
    calls := 0
    create res.make_empty
  end

feature
  put(v: TUPLE[INTEGER, INTEGER, INTEGER])
  do
    res.force(v, res.count + 1)
  end

  done()
  do
    count := count + 1
  end

  is_all_done(): BOOLEAN
  do
    calls := calls + 1
    Result := count = n
  end

  get_result(): ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
  do
    Result := res
  end

feature {NONE}
  n, count: INTEGER
  calls: INTEGER
  res: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]

end
