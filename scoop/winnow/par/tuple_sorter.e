class TUPLE_SORTER
create make
feature
  make(comparator_: TUPLE_COMPARATOR)
  do
    comparator := comparator_
  end

  sort(values: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]])
  do
    sort_impl(1, values.count + 1, values)
  end

  sort_impl(first, last: INTEGER;
    values: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]])
  local
    pivot_index, spot: INTEGER
    pivot: TUPLE[INTEGER, INTEGER, INTEGER]
  do
    if (first + 1 >= last) then
      -- return
    else
      pivot_index := (first + last) // 2
      pivot := values.item(pivot_index)
      swap(values, pivot_index, last - 1)
      spot := first
      across first |..| (last - 1) as ic loop
        if (comparator.less_than(values.item(ic.item),
              pivot)) then
          swap(values, ic.item, spot)
          spot := spot + 1
        end
      end
      swap(values, spot, last - 1)
      pivot_index := spot

      sort_impl(first, pivot_index, values)
      sort_impl(pivot_index + 1, last, values)
    end
  end

  swap(values: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]];
    i, j: INTEGER)
  local
    temp: TUPLE[INTEGER, INTEGER, INTEGER]
  do
    temp := values.item(i)
    values.put(values.item(j), i)
    values.put(temp, j)
  end

feature {NONE}
  comparator: TUPLE_COMPARATOR

end
