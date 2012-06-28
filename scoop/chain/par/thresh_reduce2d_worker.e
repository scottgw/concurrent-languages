class THRESH_REDUCE2D_WORKER
inherit EXECUTION_ENVIRONMENT
create
  make,
  make_with_filter

feature
  make (nrows_, ncols_: INTEGER; array_: separate ARRAY[INTEGER];
      aggregator_: separate THRESH_REDUCE2D_AGGREGATOR;
      op_: INTEGER)
  local
  do
    nrows := nrows_
    ncols := ncols_
    array := array_
    aggregator := aggregator_
    op := op_
  end

  make_with_filter (nrows_, ncols_: INTEGER;
      array_: separate ARRAY[INTEGER];
      aggregator_: separate THRESH_REDUCE2D_AGGREGATOR;
      op_: INTEGER;
      value_: INTEGER)
  local
  do
    nrows := nrows_
    ncols := ncols_
    array := array_
    aggregator := aggregator_
    op := op_
    value := value_
  end

feature
  live
  do
    get_result(array, aggregator)
  end

  get_result(an_array: separate ARRAY[INTEGER];
      an_aggregator: separate THRESH_REDUCE2D_AGGREGATOR)
  local
    j: INTEGER
    res: INTEGER
  do
    res := an_array.item(1)
    if op = {THRESH_REDUCE2D_OPERATOR}.filter then
      res := filter(res)
    end
    if ncols > 1 then
      across 2 |..| ncols as jc loop
        inspect op
        when {THRESH_REDUCE2D_OPERATOR}.max then
          res := res.max(an_array.item(jc.item))
        when {THRESH_REDUCE2D_OPERATOR}.filter then
          res := res + filter(an_array.item(jc.item))
        end
      end
    end
    an_aggregator.put(res)
  end

  filter(x: INTEGER): INTEGER
  do
    Result := 0
    if x = value then
      Result := 1
    end
  end

feature {NONE}
  nrows, ncols: INTEGER
  array: separate ARRAY[INTEGER]
  aggregator: separate THRESH_REDUCE2D_AGGREGATOR
  op: INTEGER
  value: INTEGER

end
