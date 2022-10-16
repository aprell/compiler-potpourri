TEST_DIRS := \
  basic_blocks \
  basic_blocks/three_address_code \
  control_flow \
  control_flow/analysis \
  data_dependences \
  data_flow/analysis \
  llvm \
  loop_transformations \
  openmp \
  qbe \
  scheduling \
  ssa

test:
	@for dir in $(TEST_DIRS); do \
	    if yes q | $(MAKE) -C $$dir $@ > /dev/null 2>&1; then \
	        printf "\e[32m[PASS]\e[0m %s\n" $$dir; \
	    else \
	        printf "\e[31m[FAIL]\e[0m %s\n" $$dir; \
	    fi \
	done

.PHONY: test
