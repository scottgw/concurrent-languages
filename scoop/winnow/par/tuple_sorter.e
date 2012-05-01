class TUPLE_SORTER
create
  make,
  make_values

feature
  make()
  do
    create comparator
    create aggregator.make
  end

  make_values(values: separate ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]])
  do
    create comparator
    create aggregator.make
    separate_values := values
  end

  sort(values: separate ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]])
    : ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
  local
    local_values: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
  do
    create local_values.make_empty
    across 1 |..| values.count as ic loop
      local_values.force(to_local_tuple(values.item(ic.item)), ic.item)
    end
    sort_impl(1, values.count + 1, local_values)
    Result := local_values
  end

  to_local_tuple(t: separate TUPLE[INTEGER, INTEGER, INTEGER])
    : TUPLE[INTEGER, INTEGER, INTEGER]
  do
    print("Sort Local Tuple before " + "%N")
    Result := [
        t.integer_32_item(1),
        t.integer_32_item(2),
        t.integer_32_item(3)]
    print("Sort Local Tuple after " + "%N")
  end

  sort_separate(first, last: INTEGER;
    parent_aggregator: separate TUPLE_SORT_AGGREGATOR)
  local
    local_values: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
  do
    print("Sort Separate begin " + first.out + " " + last.out + "%N")
    copy_values(separate_values, local_values, last, first, last)
    print("Sort Separate  after copy " + first.out + " " + last.out + "%N")

    --create local_values.make_empty
    --across first |..| (last - 1) as ic loop
      --print("Sort Separate before tuple " + first.out + " " + last.out + "%N")
      --local_values.force(to_local_tuple(values.item(ic.item)), ic.item)
      --print("Sort Separate after tuple " + first.out + " " + last.out + "%N")
    --end

    sort_impl(first, last, local_values)

    parent_aggregator.done
    --Result := local_values TODO
  end

  sort_impl(first, last: INTEGER;
    values: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]])
  local
    pivot_index, spot: INTEGER
    pivot: TUPLE[INTEGER, INTEGER, INTEGER]
    sorted_values: separate ARRAY[TUPLE[INTEGER,
      INTEGER, INTEGER]]
    left, right: separate TUPLE_SORTER
    reader: separate TUPLE_SORT_READER
  do
    print("Sort Impl " + first.out + " " + last.out + "%N")
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

      create left.make_values(left_values)
      left_launch(left, first, pivot_index)

      create right.make_values(right_values)
      right_launch(right, pivot_index + 1, last)

      create reader.make
      sorted_values := get_result(reader)

      copy_values(sorted_values, values, first, pivot_index, last)

      --left.sort_impl(first, pivot_index, values)
      --right.sort_impl(pivot_index + 1, last, values)
      --sort_impl(first, pivot_index, values)
      --sort_impl(pivot_index + 1, last, values)

    end
  end

  copy_values(
      sorted_values: separate ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]];
      values: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]];
      first, pivot_index, last: INTEGER)
  do
    print("Copy values " + pivot_index.out + " " + last.out + "%N")

    across first |..| (pivot_index - 1) as ic loop
      values.put(to_local_tuple(sorted_values.item(ic.item)), ic.item)
    end

    across pivot_index |..| (last - 1) as ic loop
      values.put(to_local_tuple(sorted_values.item(ic.item)), ic.item)
    end
  end

  get_result(reader: separate TUPLE_SORT_READER)
      : separate ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
  do
    Result := reader.get_result(aggregator)
  end

  put(values: separate ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]];
      value: TUPLE[INTEGER, INTEGER, INTEGER]; index: INTEGER)
  do
    values.force(value, index)
  end

  left_launch(sorter: separate TUPLE_SORTER; first, last: INTEGER)
  do
    print("Sort Launch left " + first.out + " " + last.out + "%N")
    sorter.sort_separate(first, last, aggregator)
    print("After Launch left " + first.out + " " + last.out + "%N")
  end

  right_launch(sorter: separate TUPLE_SORTER; first, last: INTEGER)
  do
    print("Sort Launch right " + first.out + " " + last.out + "%N")
    sorter.sort_separate(first, last, aggregator)
    print("After Launch right " + first.out + " " + last.out + "%N")
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
  aggregator: separate TUPLE_SORT_AGGREGATOR
  left_values, right_values, separate_values: separate ARRAY[TUPLE[INTEGER,
    INTEGER, INTEGER]]

end
