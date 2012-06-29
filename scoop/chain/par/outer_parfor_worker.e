class OUTER_PARFOR_WORKER
create make
feature
  make (nelts_, row_: INTEGER;
      matrix_: separate ARRAY[DOUBLE];
      points_: separate ARRAY[TUPLE[INTEGER, INTEGER]];
      aggregator_: separate OUTER_PARFOR_AGGREGATOR)
  local
  do
    nelts := nelts_
    row := row_
    matrix := matrix_
    points := points_
    aggregator := aggregator_
  end

feature
  live
  do
    get_result(matrix, points)
  end

  get_result(a_matrix: separate ARRAY[DOUBLE];
    a_points: separate ARRAY[TUPLE[INTEGER, INTEGER]])
  local
    nmax: DOUBLE
  do
    nmax := -1
    across 1 |..| nelts as jc loop
      if (not(row = jc.item)) then
        a_matrix.force(distance(a_points.item(row), a_points.item(jc.item)),
          jc.item)
        nmax := nmax.max(a_matrix.item(jc.item))
      end
    end
    a_matrix.force(nmax * nelts, row)
    done(aggregator)
  end

  done(an_aggregator: separate OUTER_PARFOR_AGGREGATOR)
  do
    an_aggregator.done
  end

  sqr(a: DOUBLE): DOUBLE
  do
    Result := a * a
  end

  distance(a, b: separate TUPLE[INTEGER, INTEGER]): DOUBLE
  do
    Result := {DOUBLE_MATH}.sqrt(
      sqr(a.integer_32_item(1) - b.integer_32_item(1)) +
      sqr(a.integer_32_item(2) - b.integer_32_item(2)));
  end

feature {NONE}
  nelts, row: INTEGER
  matrix: separate ARRAY[DOUBLE]
  points: separate ARRAY[TUPLE[INTEGER, INTEGER]]
  aggregator: separate OUTER_PARFOR_AGGREGATOR

end
