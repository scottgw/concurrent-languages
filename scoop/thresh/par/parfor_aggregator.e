class PARFOR_AGGREGATOR
create make
feature
  make (n_: INTEGER)
  local
  do
    n := n_
    count := 0
    calls := 0
  end

feature
  put
  do
    count := count + 1
    print("parfor missing " + (n - count).out + "%N")
  end

  is_all_done(): BOOLEAN
  do
    calls := calls + 1
    print("parfor is_all_done " + calls.out + "%N")
    Result := count = n
  end

feature {NONE}
  n, count: INTEGER
  calls: INTEGER

end
