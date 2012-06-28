class THRESH_REDUCE2D_AGGREGATOR
create make
feature
  make (n_, op_: INTEGER)
  local
  do
    n := n_
    op := op_
    count := 0
    res := -1
  end

feature
  put(value: INTEGER)
  do
    if count = 0 then
      res := value
    else
      inspect op
      when {THRESH_REDUCE2D_OPERATOR}.max then
        res := res.max(value)
      when {THRESH_REDUCE2D_OPERATOR}.sum then
        res := res + value
      end
    end
    count := count + 1
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
  n, op, count: INTEGER
  res: INTEGER

end
