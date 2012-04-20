class REDUCE2D_AGGREGATOR
create make
feature
  make (n_, op_: INTEGER)
  local
  do
    n := n_
    op := op_
    count := 0
    res := -1
    calls := 0
  end

feature
  put(value: INTEGER)
  do
    if count = 0 then
      res := value
    else
      inspect op
      when {REDUCE2D_OPERATOR}.max then
        res := res.max(value)
      when {REDUCE2D_OPERATOR}.sum then
        res := res + value
      else
        print("ERROR!!! %N%N%N%N")
      end
    end
    count := count + 1
    print("reduce missing " + (n - count).out + "%N")
  end

  is_all_done(): BOOLEAN
  do
    calls := calls + 1
    print("reduce is_all_done " + calls.out + "%N")
    Result := count = n
  end

  get_result(): INTEGER
  require
    is_all_done
  do
    Result := res
  end

feature {NONE}
  n, op, count: INTEGER
  res: INTEGER
  calls: INTEGER

end
