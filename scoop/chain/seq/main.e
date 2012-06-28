-- chain: chain all problems
--
-- input:
--   nelts: the number of elements
--   randmat_seed: random number generator seed
--   thresh_percent: percentage of cells to retain
--   winnow_nelts: the number of poitns to select
--
-- output:
--   res: a real vector, whose values are the result of the final product

class MAIN
inherit ARGUMENTS
create make
feature
  make
  local
    nelts, randmat_seed, thresh_percent, winnow_nelts: INTEGER
    randmat_matrix, thresh_mask: ARRAY[separate ARRAY[INTEGER]]
    winnow_points: ARRAY[TUPLE[INTEGER, INTEGER]]
    outer_matrix: ARRAY[separate ARRAY[DOUBLE]]
    outer_vector: ARRAY[DOUBLE]
    product_result: ARRAY[DOUBLE]
    randmat: RANDMAT
    thresh: THRESH
    winnow: WINNOW
    outer: OUTER
    product: PRODUCT
  do
    create in.make_open_read(separate_character_option_value('i'))

    nelts := read_integer
    randmat_seed := read_integer
    thresh_percent := read_integer
    winnow_nelts := read_integer

    create randmat_matrix.make_empty
    create thresh_mask.make_empty
    create outer_matrix.make_empty
    create outer_vector.make_empty

    create randmat.make_empty
    create thresh.make_empty
    create winnow.make_empty
    create outer.make_empty
    create product.make_empty

    randmat.randmat(nelts, nelts, randmat_seed, randmat_matrix)
    thresh.thresh(nelts, nelts, randmat_matrix, thresh_percent, thresh_mask)
    winnow_points := winnow.winnow(nelts, nelts, randmat_matrix,
        thresh_mask, winnow_nelts)
    outer.outer(winnow_nelts, winnow_points, outer_matrix, outer_vector)
    product_result := product.product(winnow_nelts, outer_matrix,
        outer_vector)

    across 1 |..| nelts as ic loop
      print(product_result.item(ic.item).out + " ")
    end
    print("%N")
  end

  read_integer(): INTEGER
  do
    in.read_integer
    Result := in.last_integer
  end

feature {NONE}
  in: PLAIN_TEXT_FILE

end -- class MAIN

