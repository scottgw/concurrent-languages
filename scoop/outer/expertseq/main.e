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
      matrix: ARRAY2[DOUBLE]
      vector: ARRAY[DOUBLE]
      file_name: STRING
      i, j: INTEGER
    do
      file_name := separate_character_option_value ('i')
      create in.make_open_read (file_name)

      nelts := read_integer
      points := read_vector_of_points(nelts)
      create matrix.make_filled(0.0, nelts, nelts)
      create vector.make_filled(0.0, 1, nelts)

      outer(nelts, points, matrix, vector)

      print(nelts.out + " " + nelts.out + "%N");
      from i := 1
      until i > nelts
      loop
        from j := 1
        until j > nelts
        loop
          print(matrix [i, j].out + " ")
          j := j + 1
        end
        print ("%N")
        i := i + 1
      end
      print("%N");

      print(nelts.out + "%N");
      from i := 1
      until i > nelts
      loop
        print (vector[i].out + " ")
        i := i + 1
      end
      print("%N");
    end

  read_integer: INTEGER
    do
      in.read_integer
      Result := in.last_integer
    end

  read_vector_of_points(nelts: INTEGER): ARRAY[TUPLE[INTEGER, INTEGER]]
    local
      i: INTEGER
    do
      create Result.make_filled([0, 0], 1, nelts)
      from i := 1
      until i > nelts
      loop
        Result [i] := [read_integer, read_integer]
        i := i + 1
      end
    end

  outer(nelts: INTEGER; points: ARRAY[TUPLE[INTEGER, INTEGER]];
        matrix: ARRAY2[DOUBLE]; vector: ARRAY[DOUBLE])
    local
      nmax: DOUBLE
      i, j: INTEGER
    do
      from i := 1
      until i > nelts
      loop
        nmax := -1
        from j := 1
        until j > nelts
        loop
          if i /= j then
            matrix [i, j] := distance (points[i], points [j])
            nmax := nmax.max(matrix [i, j])
          end
          j := j + 1
        end
        matrix [i, i] := nmax * nelts
        vector [i] :=  distance([0, 0], points [i])
        i := i + 1
      end
    end

  sqr(a: DOUBLE): DOUBLE
    do
      Result := a * a
    end

  distance(a, b: TUPLE[x, y: INTEGER]): DOUBLE
    do
      Result := {DOUBLE_MATH}.sqrt(sqr(a.x - b.x) + sqr(a.y - b.y));
    end

feature {NONE}
  in: PLAIN_TEXT_FILE

end -- class MAIN 

