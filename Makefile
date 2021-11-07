DIRS := \
  basic_blocks/three_address_code \
  basic_blocks \
  control_flow/analysis \
  data_flow/analysis \
  llvm \
  ssa

test:
	for dir in $(DIRS); do $(MAKE) -C $$dir $@; done

.PHONY: test
