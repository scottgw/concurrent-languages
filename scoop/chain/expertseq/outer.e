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

feature
  outer(nelts: INTEGER; points: ARRAY[TUPLE[INTEGER, INTEGER]];
        matrix: ARRAY2[DOUBLE]; vector: ARRAY[DOUBLE])
    local
      nmax: DOUBLE
      i, j: INTEGER
    do
      from i := 1
      until i > nelts
      loop
        from j := 1
        until j > nelts
        loop
          if i /= j then
           
            matrix [i, j] := distance(points [i], points [j])
            nmax          := nmax.max(matrix [i, j])
          end
          matrix [i, i] := nmax * nelts
          vector [i]    := distance([0, 0], points [i])
          
          j := j + 1
        end
        i := i + 1
      end
    end
  
  sqr(a: DOUBLE): DOUBLE
    do
      Result := a * a
    end
  
  distance(a, b: TUPLE[x,y: INTEGER]): DOUBLE
    do
      Result := {DOUBLE_MATH}.sqrt(sqr(a.x - b.x) + sqr(a.y - b.y));
  end

end
