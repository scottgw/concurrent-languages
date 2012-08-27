class
  TUPLE_COMPARATOR

inherit
  PART_COMPARATOR[TUPLE[INTEGER, INTEGER, INTEGER]]

feature
  less_than(u, v: TUPLE[value: INTEGER; i: INTEGER; j: INTEGER]): BOOLEAN
    local
      left_value, left_i, left_j, right_value, right_i, right_j: INTEGER
    do
      if u.value /= v.value then
        Result := u.value < v.value
      elseif u.i /= v.i then
        Result := u.i < v.i
      else
        Result := u.j < v.j
      end
    end
end
