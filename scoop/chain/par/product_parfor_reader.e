class PRODUCT_PARFOR_READER
create make
feature
  make
  do
  end

feature
  get_result(aggregator: separate PRODUCT_PARFOR_AGGREGATOR)
  require
    aggregator.is_all_done
  do
  end
end
