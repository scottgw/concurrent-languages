-- product: matrix-vector product
--
-- input:
--   matrix: a real matrix
--   vector: a real vector
--   nelts: the number of elements
--
-- output:
--   res: a real vector, whose values are the result of the product

class PRODUCT
create make_empty
feature
  make_empty do end

  product(nelts: INTEGER; matrix: ARRAY2[DOUBLE]; vector: ARRAY[DOUBLE])
    : ARRAY[DOUBLE]
  local
    res: ARRAY[DOUBLE]
    sum: DOUBLE
  do
    create res.make_filled(0.0, 1, nelts)
    across 1 |..| nelts as ic loop
      sum := 0
      across 1 |..| nelts as jc loop
        sum := sum + matrix.item(ic.item, jc.item) * vector.item(jc.item)
      end
      res.put(sum, ic.item)
    end
    Result := res
  end

end
