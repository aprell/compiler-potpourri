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

PASS := printf "\e[32m[PASS]\e[0m %s\n"
FAIL := printf "\e[31m[FAIL]\e[0m %s\n"

all: test lit_test

test:
	@for dir in $(TEST_DIRS); do \
	    if yes q | $(MAKE) -C $$dir $@ > /dev/null 2>&1; then \
	        $(PASS) $$dir; \
	    else \
	        $(FAIL) $$dir; \
	    fi \
	done

lit_test:
	@for dir in $(LIT_TEST_DIRS); do \
	    if lit $$dir > /dev/null 2>&1; then \
	        $(PASS) $$dir; \
	    else \
	        $(FAIL) $$dir; \
	    fi \
	done

.PHONY: all test lit_test
