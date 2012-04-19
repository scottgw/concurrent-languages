class REDUCE2D_AGGREGATOR
create make
feature
  make (n_: INTEGER)
  local
  do
    n := n_
    count := 0
    res := -1
  end

feature
  put(value: INTEGER)
  do
    if count = 0 then
      res := value
    else
      res := res.max(value)
    end
    count := count + 1
    print("missing " + (n - count).out + "%N")
  end

  is_all_done(): BOOLEAN
  do
    Result := count = n
  end

  get_result(): INTEGER
  require
    is_all_done
  do
    Result := res
  end

feature {NONE}
  n, count: INTEGER
  res: INTEGER

end
