class THRESH_PARFOR_READER
create make
feature
  make
  do
  end

feature
  get_result(aggregator: separate THRESH_PARFOR_AGGREGATOR)
  require
    aggregator.is_all_done
  do
  end
end
