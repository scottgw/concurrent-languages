class TUPLE_SORTER
create make
feature
  make()
  do
    create comparator
  end

  sort(values: separate ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]])
    : ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
  local
    local_values: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
  do
    create local_values.make_empty
    across 1 |..| values.count as ic loop
      local_values.force([
        values.item(ic.item).integer_32_item(1),
        values.item(ic.item).integer_32_item(2),
        values.item(ic.item).integer_32_item(3)], ic.item)
    end
    sort_impl(1, values.count + 1, local_values)
    Result := local_values
  end

  sort_separate(first, last: INTEGER;
    values: separate ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]])
  local
    local_values: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
  do
    create local_values.make_empty
    across first |..| (last - 1) as ic loop
      local_values.force([
        values.item(ic.item).integer_32_item(1),
        values.item(ic.item).integer_32_item(2),
        values.item(ic.item).integer_32_item(3)], ic.item)
    end
    sort_impl(first, last, local_values)
    --Result := local_values TODO
  end

  sort_impl(first, last: INTEGER;
    values: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]])
  local
    pivot_index, spot: INTEGER
    pivot: TUPLE[INTEGER, INTEGER, INTEGER]
    left_values, right_values: separate ARRAY[TUPLE[INTEGER,
      INTEGER, INTEGER]]
    left, right: separate TUPLE_SORTER
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

      create left_values.make_empty
      across first |..| (pivot_index - 1) as ic loop
        put(left_values, values.item(ic.item), ic.item)
      end

      create right_values.make_empty
      across pivot_index |..| (last - 1) as ic loop
        put(right_values, values.item(ic.item), ic.item)
      end

      create left.make
      launch(left, first, pivot_index, left_values)

      create right.make
      launch(right, pivot_index + 1, last, right_values)

      --left.sort_impl(first, pivot_index, values)
      --right.sort_impl(pivot_index + 1, last, values)
      sort_impl(first, pivot_index, values)
      sort_impl(pivot_index + 1, last, values)
    end
  end

  put(values: separate ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]];
      value: TUPLE[INTEGER, INTEGER, INTEGER]; index: INTEGER)
  do
    values.force(value, index)
  end

  launch(sorter: separate TUPLE_SORTER; first, last: INTEGER;
    values: separate ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]])
  do
    sorter.sort_separate(first, last, values)
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
  --aggregator: separate TUPLE_SORT_AGGREGATOR

end
