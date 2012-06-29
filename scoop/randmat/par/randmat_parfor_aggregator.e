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
    --print("put -> missing " + (n - count).out + "%N")
    --if count = n then
      --print("%N%N%NALL DONE!%N%N%N")
      --done(main)
      --print("aggregator dead%N")
    --end
  end

  is_all_done(): BOOLEAN
  do
    -- print("is_all_done -> missing " + (n - count).out)
    Result := count = n
    --if Result then
      --print("is_all_done%N")
    --end
  end

feature {NONE}
  n, count: INTEGER
  main : separate MAIN

end
