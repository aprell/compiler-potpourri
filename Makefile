TEST_DIRS := \
  basic_blocks/three_address_code \
  basic_blocks \
  control_flow/analysis \
  data_flow/analysis \
  llvm \
  qbe \
  ssa

LIT_TEST_DIRS := \
  control_flow \
  data_dependences \
  loop_transformations \
  scheduling

all: test lit_test

test:
	@for dir in $(TEST_DIRS); do $(MAKE) -C $$dir $@; done

lit_test:
	@for dir in $(LIT_TEST_DIRS); do lit $$dir; done

.PHONY: all test lit_test
