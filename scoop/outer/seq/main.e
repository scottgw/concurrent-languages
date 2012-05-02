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

class MAIN
inherit ARGUMENTS
create make
feature
  make
  local
    nelts: INTEGER
    points: ARRAY[TUPLE[INTEGER, INTEGER]]
    matrix: ARRAY2[REAL]
    vector: ARRAY[REAL]
    i, j: INTEGER
    file_name: STRING
  do
    file_name := separate_character_option_value('i')
    create in.make_open_read(separate_character_option_value('i'))

    nelts := read_integer
    points := read_vector_of_points(nelts)
    create matrix.make_filled(0, nelts, nelts)
    create vector.make_filled(0, 1, nelts)

    outer(nelts, points, matrix, vector)

    print(nelts.out + " " + nelts.out + "%N");
    across 1 |..| nelts as ic loop
      across 1 |..| nelts as jc loop
        print(matrix.item(ic.item, jc.item).out + " ");
      end
      print("%N");
    end
    print("%N");

    print(nelts.out + "%N");
    across 1 |..| nelts as ic loop
      print(vector[i].out + " ");
    end
    print("%N");
  end

  read_integer(): INTEGER
  do
    in.read_integer
    Result := in.last_integer
  end

  read_vector_of_points(nelts: INTEGER): ARRAY[TUPLE[INTEGER, INTEGER]]
  local
    i, j: INTEGER
    vector: ARRAY[TUPLE[INTEGER, INTEGER]]
  do
    create vector.make_filled([0, 0], 1, nelts)
    across 1 |..| nelts as ic loop
      vector.put([read_integer, read_integer], ic.item)
    end
    Result := vector
  end

  outer(nelts: INTEGER; points: ARRAY[TUPLE[INTEGER, INTEGER]];
      matrix: ARRAY2[REAL]; vector: ARRAY[REAL])
  local
    nmax: REAL
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

  sqr(a: REAL): REAL
  do
    Result := a * a
  end

  distance(a, b: TUPLE[INTEGER, INTEGER]): REAL
  do
    Result :=(sqr(a.integer_32_item(1) - b.integer_32_item(1)) +
        a.integer_32_item(2) - b.integer_32_item(2)).sqrt();
  end

feature {NONE}
  in: PLAIN_TEXT_FILE

end -- class MAIN 
