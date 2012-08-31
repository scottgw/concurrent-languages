-- randmat: random number generation
--
-- input:
--   nrows, ncols: the number of rows and cols
--   s: the seed
--
-- output:
--   matrix: a nrows by ncols integer matrix

class RANDMAT
create make_empty
feature
  make_empty do end

  randmat(nrows, ncols:INTEGER;  s: NATURAL; matrix: ARRAY2[INTEGER])
    local
      seed, lcg_a, lcg_c, rand_max: NATURAL
      i, j: INTEGER
    do
      lcg_a := 1664525
      lcg_c := 1013904223
      rand_max := 100

      from i := 1
      until i > nrows
      loop
        seed := s + i.to_natural_32 - 1
        from j := 1
        until j > ncols
        loop
          seed := lcg_a * seed + lcg_c
          matrix [i, j] := (seed \\ rand_max).to_integer_32
          j := j + 1
        end
        i := i + 1
      end
    end

end
