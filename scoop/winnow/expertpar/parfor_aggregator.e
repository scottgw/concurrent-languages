class PARFOR_AGGREGATOR
create make
feature
  make (n_: INTEGER)
  local
  do
    n := n_
    count := 0
    create res.make_empty
  end

feature
  put(v: separate TUPLE[INTEGER, INTEGER, INTEGER])
  do
    res.force([v.integer_32_item(1), v.integer_32_item(2), v.integer_32_item(3)], res.count + 1)
  end

  done()
  do
    count := count + 1
  end

  is_all_done(): BOOLEAN
  do
    Result := count = n
  end

  get_result(): ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
  do
    Result := res
  end

feature {NONE}
  n, count: INTEGER
  res: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]

end
