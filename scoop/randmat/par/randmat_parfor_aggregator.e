class RANDMAT_PARFOR_AGGREGATOR
create make
feature
  make (n_: INTEGER; main_: separate MAIN)
  local
  do
    n := n_
    count := 0
    main := main_
  end

feature
  put
  do
    count := count + 1
    --print("put " + count.out + "%N")
  end

  is_all_done(): BOOLEAN
  do
    Result := count = n
  end

feature {NONE}
  n, count: INTEGER
  main : separate MAIN

end
