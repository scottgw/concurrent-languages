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

  randmat(nrows, ncols, s: INTEGER; matrix: ARRAY2[INTEGER])
  local
    seed, lcg_a, lcg_c, rand_max, int_max: INTEGER
  do
    lcg_a := 1664525
    lcg_c := 1013904223
    rand_max := 100
    int_max := 2147483647
    across 1 |..| nrows as ic loop
      seed := s + ic.item
      across 1 |..| ncols as jc loop
        seed := (lcg_a * seed + lcg_c) \\ int_max
        matrix.put((((seed \\ rand_max) + rand_max) \\ rand_max),
            ic.item, jc.item)
      end
    end
  end

end
