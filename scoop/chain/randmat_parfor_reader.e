class RANDMAT_PARFOR_READER
create make
feature
  make
  do
  end

feature
  get_result(aggregator: separate RANDMAT_PARFOR_AGGREGATOR)
  require
    aggregator.is_all_done
  do
  end
end
