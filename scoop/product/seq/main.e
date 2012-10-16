-- product: matrix-vector product
--
-- input:
--   matrix: a real matrix
--   vector: a real vector
--   nelts: the number of elements
--
-- output:
--   res: a real vector, whose values are the result of the product

class MAIN
inherit ARGUMENTS
create make
feature
  make
  local
    nelts: INTEGER
    matrix: ARRAY2[DOUBLE]
    vector: ARRAY[DOUBLE]
    res: ARRAY[DOUBLE]
  do
    create in.make_open_read(separate_character_option_value('i'))
    is_bench := index_of_word_option ("bench") > 0

    in.read_integer
    nelts := in.last_integer

    create matrix.make_filled(0.0, nelts, nelts)
    if not is_bench then
      across 1 |..| nelts as ic loop
        across 1 |..| nelts as jc loop
          matrix.put(read_double(), ic.item, jc.item)
        end
      end
    end

    create vector.make_filled(0.0, 1, nelts)
    if not is_bench then
      across 1 |..| nelts as ic loop
        vector.put(read_double(), ic.item)
      end
    end

    res := product(nelts, matrix, vector)

    if not is_bench then
      print(nelts.out + "%N");
      across 1 |..| nelts as ic loop
        print(res.item(ic.item).out + " ");
      end
      print("%N");
    end
  end

  is_bench: BOOLEAN

  read_double(): DOUBLE
  do
    in.read_double
    Result := in.last_double
  end

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

feature {NONE}
  in: PLAIN_TEXT_FILE

end -- class MAIN 

