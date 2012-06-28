-- outer: outer product
--
-- input:
--   vector: a vector of (x, y) points
--   nelts: the number of points
--
-- output:
--   matrix: a real matrix, whose values are filled with inter-point
--     distances
--   vector: a real vector, whose values are filled with origin-to-point
--     distances

class OUTER
create make_empty
feature
  make_empty do end

  outer(nelts: INTEGER; points: ARRAY[TUPLE[INTEGER, INTEGER]];
      matrix: ARRAY2[DOUBLE]; vector: ARRAY[DOUBLE])
  local
    nmax: DOUBLE
  do
    across 1 |..| nelts as ic loop
      nmax := -1
      across 1 |..| nelts as jc loop
        if (not(ic.item = jc.item)) then
          matrix.put(distance(points.item(ic.item), points.item(jc.item)),
            ic.item, jc.item)
          nmax := nmax.max(matrix.item(ic.item, jc.item))
        end
        matrix.put(nmax * nelts, ic.item, ic.item)
        vector.put(distance([0, 0], points.item(ic.item)), ic.item)
      end
    end
  end

  sqr(a: DOUBLE): DOUBLE
  do
    Result := a * a
  end

  distance(a, b: TUPLE[INTEGER, INTEGER]): DOUBLE
  do
    Result := {DOUBLE_MATH}.sqrt(
      sqr(a.integer_32_item(1) - b.integer_32_item(1)) +
      sqr(a.integer_32_item(2) - b.integer_32_item(2)));
  end

end
