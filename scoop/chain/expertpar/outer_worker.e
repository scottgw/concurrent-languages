class
  PARFOR_WORKER

create
  make

feature
  make (start_, final_, nelts_: INTEGER;
        x_points_: separate ARRAY[INTEGER];
        y_points_: separate ARRAY[INTEGER];
        result_vector_: separate ARRAY[DOUBLE];
        result_matrix_: separate ARRAY2[DOUBLE])
    do
      start := start_
      final := final_
      nelts := nelts_
      x_points := x_points_
      y_points := y_points_
      result_vector := result_vector_
      result_matrix := result_matrix_
    end

feature
  
  live
    local
      a: ARRAY [TUPLE [INTEGER, INTEGER]]
    do
      a := fetch_array (x_points, y_points)
      get_result(a)
    end


  fetch_array (xs, ys: separate ARRAY[INTEGER]):
      ARRAY [TUPLE[INTEGER, INTEGER]]
    local
      i: INTEGER
      x, y: INTEGER
    do
      create Result.make (1, nelts)

      from i := 1
      until i > nelts
      loop
        -- SCOOP bug: this doesn't work if I don't store these into
        -- local variables explicitly.
        x := xs [i]
        y := ys [i]
        Result [i] := [x, y]
        i := i + 1
      end
    end

  to_local_row (i: INTEGER): INTEGER
    do
      Result := i - start + 1
    end

  
  get_result(a_points: ARRAY[TUPLE[x,y: INTEGER]])
    local
      nmax: DOUBLE
      d: DOUBLE
      p1, p2: TUPLE [x,y : INTEGER]
      i, j: INTEGER
      matrix: ARRAY2[DOUBLE]
      vector: ARRAY [DOUBLE]
    do
      create matrix.make (to_local_row (final), nelts)
      create vector.make (1, to_local_row (final))

      from i := start
      until i > final
      loop
        nmax := -1
        p1 := a_points [i]
        from j := 1
        until j > nelts
        loop
          if i /= j then
            p2 := a_points [j]
            d := distance (p1, p2)
            matrix [to_local_row (i), j] := d
            nmax := nmax.max (d)
          end
          j := j + 1
        end
        matrix [to_local_row (i), i] := nmax * nelts
        vector [to_local_row (i)] := distance ( [0,0], a_points [i])
        i := i + 1
      end
      print ("set result vector%N")
      set_result_vector (vector, result_vector)
      print ("set result matrix%N")
      set_result_matrix (matrix, result_matrix)
    end

  set_result_matrix (mat: ARRAY2[DOUBLE]; smat: separate ARRAY2[DOUBLE])
    require
      smat.generator /= Void
    local
      i, j: INTEGER
    do
      -- print ("size: " + (final - start + 1).out + " nelts: " + nelts.out + "%N")
      from i := start
      until i > final
      loop
        from j := 1
        until j > nelts
        loop
          smat [i, j] := mat [to_local_row (i), j] -- put (mat [to_local_row (i), j], i, j)
          smat [i, j].do_nothing
          j := j + 1
        end
        i := i + 1
      end
    end

  set_result_vector (vec: ARRAY[DOUBLE]; svec: separate ARRAY[DOUBLE])
    local
      i, j: INTEGER
    do
      from i := start
      until i > final
      loop
        svec [i] := vec [to_local_row (i)]
        svec [i].do_nothing
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

feature {NONE}
  start, final, nelts: INTEGER
  result_vector: separate ARRAY[DOUBLE]
  result_matrix: separate ARRAY2[DOUBLE]
  x_points, y_points: separate ARRAY[INTEGER]

end
