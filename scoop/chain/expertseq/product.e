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

feature
  product(nelts: INTEGER; matrix: ARRAY2[DOUBLE]; vector: ARRAY[DOUBLE])
    : ARRAY[DOUBLE]
    local
      i, j: INTEGER
      sum: DOUBLE
    do
      create Result.make_filled(0.0, 1, nelts)
          
      from i := 1
      until i > nelts
      loop
        sum := 0
        
        from j := 1
        until j > nelts
        loop
          sum := sum + matrix [i, j] * vector [j]
          
          j := j + 1
        end
        Result [i] := sum
        
        i := i + 1
      end
    end
  
end
