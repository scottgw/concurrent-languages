class REDUCE2D_AGGREGATOR
create make
feature
  make (n_: INTEGER)
  local
  do
    print("aggregator n = " + n_.out + "%N")
    n := n_
    count := 0
    res := -1
  end

feature
  put(value: INTEGER)
  do
    print("put " + value.out + ", count: " + count.out + "%N")
    if count = 0 then
      res := value
    else
      res := res.max(value)
    end
    count := count + 1
  end

  is_all_done(): BOOLEAN
  do
    Result := count = n
    print("inside is_all_done = " + Result.out + "%N")
  end

  get_result(): INTEGER
  require
    is_all_done
  do
    print("count = " + count.out + ", n = " + n.out + "%N")
    print("is_all_done() = " + is_all_done().out + "%N")
    print("get_result = " + res.out + "%N")
    Result := res
  end

feature {NONE}
  n, count: INTEGER
  res: INTEGER

end
