class PARFOR_AGGREGATOR
create make
feature
  make (n_: INTEGER)
  local
  do
    n := n_
    count := 0
  end

feature
  done()
  do
    count := count + 1
  end

  is_all_done(): BOOLEAN
  do
    Result := count = n
  end

  get_result()
  do
  end

feature {NONE}
  n, count: INTEGER

end
